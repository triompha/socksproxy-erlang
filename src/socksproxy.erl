-module(socksproxy).
-export([start/0,stop/0]).

%这是erlang进程第一个调用的Module
%当启动进程的时候的命令是  erl -s socksproxy 
%就会调用 socksproxy.erl的start方法进行启动

start()->
	%当调用这个方法的时候，会查找当前目录的"socksproxy_app.src"
	%查找 mod 里面的定义  发现："{mod, { socksproxy_app, []}}"
	%就会查找 "socksproxy_app.erl"，调用这个module的start(_StartType, _StartArgs)方法
	application:start(socksproxy).

stop()->
	application:stop(socksproxy).
