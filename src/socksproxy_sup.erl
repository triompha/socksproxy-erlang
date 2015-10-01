-module(socksproxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([Socks5LSock]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Socks5LSock]).

start_child() ->
	supervisor:start_child(?MODULE,[]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Socks5LSock]) ->
	Child={socksproxy_child,
		   	{socksproxy_child, start_link, [Socks5LSock]},
			temporary, brutal_kill, worker, 
			[socksproxy_child]},

	
    {ok, { {simple_one_for_one, 0, 1}, [Child]} }.

