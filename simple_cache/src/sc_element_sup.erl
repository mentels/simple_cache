-module(sc_element_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    supervisor:start_child(?MODULE, [Value, LeaseTime]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = {simple_one_for_one, 0, 1},
    Element = {sc_element, {sc_element, start_link, []}, temporary,
               brutal_kill, worker, [sc_element]},
    Children = [Element],
    {ok, {RestartStrategy , Children}}.

