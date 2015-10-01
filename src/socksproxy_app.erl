-module(socksproxy_app).

%Application behaviour
%其他地方调用application:start(socksproxy_app),就会调用，这个module的start方法
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	%配置文件在conf.config
	{ok,SocksPort} = application:get_env(socksproxy,port),
	{ok,Socks5LSock} = gen_tcp:listen(SocksPort,
			[binary,{reuseaddr,true},{active,false},{backlog,256}]),

	io:format("[~p]Listen on:~p  ~n",[?MODULE,SocksPort]),
    
	% 因为supervisour采用的是 simple_one_for_one 模式 
	% 因为socks_child不采用重启模式，每次都新生成一个加到父节点中
	% 这里控制sup的启动和第一个子worker的启动
	case socksproxy_sup:start_link([Socks5LSock]) of
		{ok,Pid} ->
			socksproxy_sup:start_child(),
			io:format("[~p]Start socks server ok! ~n",[?MODULE]),
			{ok,Pid};
		Other ->
			io:format("[~p]Start socks server error:~p! ~n",[?MODULE,Other]),
			{error,Other}
	end.

stop(_State) ->
	io:format("[~p]Application stopped state:~p! ~n",[?MODULE,_State]),
	ok.

