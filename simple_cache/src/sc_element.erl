-module(sc_element).

-behaviour(gen_server).

-export([start_link/2,
         create/2,
         create/1,
         fetch/1,
         replace/2,
         delete/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)). %% a day

-record(state, {value, lease_time, start_time}).

%% API
start_link(Value, LeaseTime) ->
    gen_server:start_link(?SERVER, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    sc_element_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

%% Callbacks
init([Value, LeaseTime]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    Timeout = time_left(StartTime, LeaseTime),
    {ok, #state{value = Value,
                lease_time = LeaseTime,
                start_time = StartTime}, Timeout}.

handle_call(fetch, _From, #state{value = V,
                                 lease_time = LT,
                                 start_time = ST} = State) ->
    Timeout = time_left(ST, LT),
    {reply, {ok, V}, State, Timeout}.

handle_cast({replace, Value}, #state{lease_time = LT,
                                     start_time = ST} = State) ->
    Timeout = time_left(ST, LT),
    {noreply, State#state{value = Value}, Timeout};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    Key = sc_store:key(self()),
    sc_event:timeout(Key),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    sc_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helpers
time_left(_, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    ElapsedTime = CurrentTime - StartTime,
    case LeaseTime - ElapsedTime of
        T when T =< 0 ->
            0;
        T ->
            T * 1000 %% we want it in miliseconds
    end.

