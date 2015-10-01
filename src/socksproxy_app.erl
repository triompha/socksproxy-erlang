-module(socksproxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok,SocksPort} = application:get_env(socksproxy,port),
	{ok,Socks5LSock} = gen_tcp:listen(SocksPort,[binary,{reuseaddr,true},{active,false},{backlog,256}]),

	% supervisour one_for_one 模式
%	socksproxy_sup:start_link([Socks5LSock]).

    % supervisour simple_one_for_one 模式 
	case socksproxy_sup:start_link([Socks5LSock]) of
		{ok,Pid} ->
			socksproxy_sup:start_child(),
			io:format("~p ok .... ~n",[Pid]),
			{ok,Pid};
		Other ->
			io:format("~p error .... ~n",[Other]),
			{error,Other}
	end.

stop(_State) ->
	io:format("application exit state:~p",[_State]),
	ok.

