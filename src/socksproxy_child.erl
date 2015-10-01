-module(socksproxy_child).
-behavior(gen_server).
-export([start_link/1]).


-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {lsock, socket, remote}).

-define(TIMEOUT, 1000 * 60 * 10).
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

start_link(Socks5LSock) ->
	gen_server:start_link(?MODULE,[Socks5LSock],[]).


init([SLSock]) ->
	{ok,#state{lsock=SLSock},0}.



handle_call(_Request,_From,State)->
	io:format("receive handle_call request: ~p , from: ~p, state:~p ~n",[_Request,_From,State]),
	Reply = ok,
	{reply,Reply,State}.

handle_cast(_Info,State) ->
	io:format("receive handle_cast info: ~p , state:~p ~n",[_Info,State]),
	{noreply,State}.

handle_info(timeout,#state{lsock=LSock,socket=undefined}=State) ->
	io:format("receive handle_info-timeout1,State:~p  ~n",[State]),
	{ok,Socket}=gen_tcp:accept(LSock),
	socksproxy_sup:start_child(),
	case start_process(Socket) of
        {ok, Remote} ->
			io:format("connected remote : ~p  ~n",[Remote]),
            ok = inet:setopts(Socket, [{active, once}]),
            ok = inet:setopts(Remote, [{active, once}]),
            {noreply, State#state{socket=Socket, remote=Remote}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;


handle_info(timeout, #state{lsock = LSock,socket=Socket,remote=undefined} = State)->
	io:format("receive handle_info-timeout-null LSock: ~p ~n",[LSock]),
	{stop, timeout, State};

handle_info(timeout, #state{socket = Socket} = State) when is_port(Socket) ->
	io:format("receive handle_info-timeout-null ~n",[]),
	{stop, timeout, State};


handle_info({tcp, Socket, Request}, #state{socket=Socket, remote=Remote} = State) ->
	io:format("receive handle_info-tcp Remote: ~p , Request:~p ~n",[Remote,Request]),
	    case gen_tcp:send(Remote, Request) of
		        ok ->
			            %ok = inet:setopts(Remote, [{active, once}]),
						io:format("send handle_info-tcp ok ~n",[]),
			            {noreply, State, ?TIMEOUT};
		        {error, Error} ->
			            {stop, Error, State}
			    end;

handle_info({tcp, Socket, Response}, #state{socket=Client, remote=Socket} = State) ->
	io:format("receive handle_info-tcp Remote: ~p  ~n",[Socket]),
    case gen_tcp:send(Client, Response) of
		ok ->
            ok = inet:setopts(Client, [{active, once}]),
		   	ok = inet:setopts(Socket, [{active, once}]),
        	{noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
   end;

handle_info({tcp_closed,_},State)->
	io:format("receive handle_info-tcp_closed State: ~p  ~n",[State]),
	{stop,normal,State};

handle_info({tcp_error,_, Reason},State) ->
	io:format("receive handle_info-tcp_error Reason: ~p  ~n",[Reason]),
	{stop,Reason,State}.

terminate(_Reason, #state{socket=Socket, remote=Remote}) ->
	io:format("receive terminate-_Reason: ~p  ~n",[_Reason]),
	    case is_port(Socket) of
		        true -> gen_tcp:close(Socket);
		        false -> ok
			    end,

	    case is_port(Remote) of
		        true -> gen_tcp:close(Remote);
		        false -> ok
			    end.

code_change(_OldVsn, State, _Extra) ->
	    {ok, State}.



start_process(Socket) ->
    {ok, Address,Port} = read_socks5_request(Socket),
	io:format("start_process: ~p  ~n",[Address++":"++Port]),
	case gen_tcp:connect(Address, Port, [binary, {active, true}]) of
        {ok, RemoteSocket} ->
            ok = gen_tcp:send(Socket, <<5, 0, 0, 1, <<0,0,0,0>>/binary, 0:16>>),
            {ok, RemoteSocket};
        {error, Error} ->
            {error, Error}
    end.



find_target(Socket) ->
 	{ok, <<5:8, Nmethods:8>>}=gen_tcp:recv(Socket,2),
	io:format("receive  nmethods: ~p  ~n",[Nmethods]),
	{ok, _Methods} = gen_tcp:recv(Socket, Nmethods),
	gen_tcp:send(Socket, <<5:8/integer, 0:8/integer>>),
	{ok, <<5:8, 1:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Socket, 4),

	case AType of
		?IPV4 ->
			{ok, <<Address:32>>} = gen_tcp:recv(Socket, 4),
			{ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
			{ok, <<?IPV4, Port:16, Address:32>>};
		?IPV6 ->
			{ok, <<Address:128>>} = gen_tcp:recv(Socket, 16),
			{ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
			{ok, <<?IPV6, Port:16, Address:128>>};
		?DOMAIN ->
			{ok, <<DomainLen:8>>} = gen_tcp:recv(Socket, 1),
			{ok, <<DomainBin/binary>>} = gen_tcp:recv(Socket, DomainLen),
			{ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
			{ok, <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>}
	end.

read_socks5_request(ClientSocket) ->
	case find_target(ClientSocket) of
			{error, Reason} ->
					{error, Reason};
			{ok, Target} ->
					case Target of
							<<?IPV4, InnerPort:16, Address/binary>> ->
									InnerAddr = inet_parse:ntoa(list_to_tuple(binary_to_list(Address))),
									{ok, InnerAddr, InnerPort};
							<<?IPV6, _InnerPort:16, _Address/binary>> ->
									{error, "not support ipv6"};
							<<?DOMAIN, InnerPort:16, DomainLen:8, DomainBin:DomainLen/binary>> ->
									Address = binary_to_list(DomainBin),
									{ok, Address, InnerPort}
							end
			end.
