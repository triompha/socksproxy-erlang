-module(socksproxy_child).
-behavior(gen_server).
-export([start_link/1]).

%%Gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%定义一些socks的基本协议
-define(TIMEOUT, 1000 * 60 * 10).
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-record(state, {lsock, socket, remote}).

%supervisor的子进程的调用方法
start_link(Socks5LSock) ->
	gen_server:start_link(?MODULE,[Socks5LSock],[]).

%gen_server的回调初始化方法
init([SLSock]) ->
	{ok,#state{lsock=SLSock},0}.



handle_call(_Request,_From,State)->
	io:format("[~p]handle_call request: ~p , from: ~p, state:~p ~n",[?MODULE,_Request,_From,State]),
	Reply = ok,
	{reply,Reply,State}.

handle_cast(_Info,State) ->
	io:format("[~p]handle_cast info: ~p , state:~p ~n",[?MODULE,_Info,State]),
	{noreply,State}.

handle_info(timeout,#state{lsock=LSock,socket=undefined}=State) ->
	io:format("[~p]handle_info:timeout,socket undefied, state:~p ~n",[?MODULE,State]),
	%接受端口请求
	{ok,Socket}=gen_tcp:accept(LSock),
	%手动加载子节点
	socksproxy_sup:start_child(),
	case socks_handshake(Socket) of
        {ok, Remote} ->
			io:format("[~p]handle_info:timeout,socket=undefied connected to remote ~p, state:~p ~n",[?MODULE,State,Remote]),
            ok = inet:setopts(Socket, [{active, once}]),
            ok = inet:setopts(Remote, [{active, once}]),
            {noreply, State#state{socket=Socket, remote=Remote}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;


handle_info(timeout, #state{socket = Socket,remote=undefined} = State) ->
	io:format("[~p]handle_info:timeout,remote=undefied Socket:~p, state:~p ~n",[?MODULE,Socket,State]),
	{stop, timeout, State};

%处理来自socks5的request请求
handle_info({tcp, Socket, Request}, #state{socket=Socket, remote=Remote} = State) ->
	io:format("[~p]handle_info:tcp request, state:~p ~n",[?MODULE,State]),
	case gen_tcp:send(Remote, Request) of
		ok ->
			io:format("[~p]send request to remote ok! state:~p ~n",[?MODULE,State]),
			{noreply, State, ?TIMEOUT};
		{error, Error} ->
			{stop, Error, State}
	end;

handle_info({tcp, Socket, Response}, #state{socket=Client, remote=Socket} = State) ->
	%io:format("[~p]handle_info:tcp response, state:~p ~n",[?MODULE,State]),
    case gen_tcp:send(Client, Response) of
		ok ->
            ok = inet:setopts(Client, [{active, once}]),
		   	ok = inet:setopts(Socket, [{active, once}]),
	%		io:format("[~p]send response to socks ok! state:~p ~n",[?MODULE,State]),
        	{noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
   end;

handle_info({tcp_closed,_},State)->
	io:format("[~p]handle_info:tcp_closed response, state:~p ~n",[?MODULE,State]),
	{stop,normal,State};

handle_info({tcp_error,_, Reason},State) ->
	io:format("[~p]handle_info:tcp_error response, state:~p ~n",[?MODULE,State]),
	{stop,Reason,State}.

terminate(_Reason, #state{socket=Socket, remote=Remote}=State) ->
	io:format("[~p]terminate reason:~p, state:~p ~n",[?MODULE,_Reason,State]),
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



%与socks5客户端的握手，不需要账号密码，
%返回结果为已经连接的socks想连接的IP,PORT的socket
socks_handshake(Socket) ->
    {ok, Address,Port} = get_socks_destnation(Socket),
	io:format("[~p]socks_handshake Socket:~p got Address:~p,Port:~p ~n",[?MODULE,Socket,Address,Port]),
	case gen_tcp:connect(Address, Port, [binary, {active, true}]) of
        {ok, RemoteSocket} ->
			io:format("[~p]socks_handshake Socket:~p connect to Address:~p,Port:~p ok! ~n",[?MODULE,Socket,Address,Port]),
            ok = gen_tcp:send(Socket, <<5, 0, 0, 1, <<0,0,0,0>>/binary, 0:16>>),
            {ok, RemoteSocket};
        {error, Error} ->
			io:format("[~p]socks_handshake Socket:~p connect to Address:~p,Port:~p error! :~p ~n",[?MODULE,Socket,Address,Port,Error]),
            {error, Error}
    end.

get_socks_destnation(Socket) ->
 	{ok, <<5:8, Nmethods:8>>}=gen_tcp:recv(Socket,2),
	io:format("receive  nmethods: ~p  ~n",[Nmethods]),
	{ok, _Methods} = gen_tcp:recv(Socket, Nmethods),
	gen_tcp:send(Socket, <<5:8/integer, 0:8/integer>>),
	{ok, <<5:8, 1:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Socket, 4),

	case AType of
		?IPV4 ->
			{ok, <<Address:32>>} = gen_tcp:recv(Socket, 4),
			{ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
			{ok,<<?IPV4, DPort:16, DAddress/binary>>} = {ok, <<?IPV4, Port:16, Address:32>>},
			InnerAddr = inet_parse:ntoa(list_to_tuple(binary_to_list(DAddress))),
			{ok, InnerAddr, DPort};
		?IPV6 ->
			%{ok, <<Address:128>>} = gen_tcp:recv(Socket, 16),
			%{ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
			{error, "not support ipv6"};
		?DOMAIN ->
			{ok, <<DomainLen:8>>} = gen_tcp:recv(Socket, 1),
			{ok, <<DomainBin/binary>>} = gen_tcp:recv(Socket, DomainLen),
			{ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
			Address = binary_to_list(DomainBin),
			{ok, Address, Port}
	end.

