%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc Worker example
%%% @end
%%%-------------------------------------------------------------------
-module(worker).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(worker_state, {pause = false, uuid, args, time, points = [], acc = []}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(UUID, Args, Route) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [{UUID, Args, Route}], []).

init(Params) ->
  {UUID, Args, Route} = Params,
  io:format("Starting worker "),
  io:format(UUID),
  io:format("~n"),
  State = #worker_state{pause = false, args = Args, uuid = UUID, points = Route, acc = []},
  self() ! start_simulation,
  {ok, State}.


handle_call(_Request, _From, State = #worker_state{}) ->
  {reply, ok, State}.

handle_cast({pause, true}, State = #worker_state{}) ->
  State#worker_state{pause = true},
  {noreply, State};

handle_cast({pause, false}, State = #worker_state{}) ->
  State#worker_state{pause = false},
  {noreply, State};

handle_cast(_Request, State = #worker_state{}) ->
  {noreply, State}.

handle_info(start_simulation, State = #worker_state{}) ->
  io:format("Start ~n "),
  simulate(State#worker_state.points,
    State#worker_state.acc, State#worker_state.args,
    State#worker_state.time, State#worker_state.pause),
  {noreply, State};

handle_info(_Info, State = #worker_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #worker_state{}) ->
  result_handler_server ! {stop_reporting,
    {uuid, _State#worker_state.uuid}, {reason, _Reason}, {time, _State#worker_state.time}},
    ok.

code_change(_OldVsn, State = #worker_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

simulate([], _, _, Time, pause = false) ->
  result_handler_server ! {finished, {process, self()},{uuid, #worker_state.uuid}, {time, Time}},
  ok;

simulate([Point |Route], Acc , Args, Time, pause = false) ->
  result_handler_server ! calculate(Args, Point, Time),
  #worker_state{points = Route, acc = [Point | Acc], time = Time + 1},
  simulate(Route, [Point | Acc], Args, Time + 1, #worker_state.pause);

simulate([], _, _, Time, pause = true) ->
  #worker_state{points = [], time = Time},
  ok;

simulate([Point |Route], Acc , _, Time, pause = true) ->
  #worker_state{points = Route, acc = [Point | Acc], time = Time + 1},
  ok.

%%Calculating simulation
calculate(Args, Point, Time) ->
  %%%-------------------------------------------------------------------
  %%%
  %%% {args, {value_name_1, value_1}, {value_name_2, value_2}, {....}, {value_name_n, value_n}} = Args,
  %%%
  %%%-------------------------------------------------------------------
  {func_args, {speed, Speed_value}, {fuel, Fuel_value}} = Args,
  {calculation, {simulation,{uuid, #worker_state.uuid}},
    {point, Point},
    %%%-----------------------------------------------------------------
    %%%
    %%%   {parameter, value + value * math:function(Time + random:uniform(rand:normal_s(Time)))},
    %%%    ...
    %%%
    %%%
    %%%-----------------------------------------------------------------
    {speed, Speed_value + Speed_value * math:sin(Time + rand:uniform(rand:normal_s(Time)))},
    {fuel, Fuel_value + Fuel_value * math:cos(Time + rand:uniform(rand:normal_s(Time)))}}.
