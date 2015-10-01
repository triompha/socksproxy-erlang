-module(socksproxy).
-export([start/0,stop/0]).



start()->
	application:start(socksproxy).


stop()->
	application:stop(socksproxy).
