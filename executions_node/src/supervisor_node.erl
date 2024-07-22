%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(supervisor_node).

-behaviour(supervisor).

-export([start_link/1, init/1, pause/0, stop/0, continue/0]).
-record(simulation_params, { uuid_workers = [] ,arguments_of_functions, route, module_name}).
-record(state, {worker_pids = []}).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  {args, _,
    {worker_id ,UUID},
    Arguments_of_functions,
    {route, Route},
    {module_name ,Module_name}
  } = Args,

  Children = [#{
    id => {child_process, Index},
    start => {
        worker,
        start_link, [get_element(Index, UUID),
        Arguments_of_functions, Route]
    },
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [erlang:binary_to_atom(Module_name)],
    monitor => {process, true}}
    || Index <- lists:seq(1, length(UUID))],

  #state{worker_pids = Children},
  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    Children}
  }.

pause() ->
  Children = #state.worker_pids,
  [pause(Child) || Child <- Children].

pause(Child) ->
  gen_server:cast(Child, {pause, true}).

stop() ->
  Children = #state.worker_pids,
  [supervisor:terminate_child(self(), Child) || Child <- lists:reverse(Children)].

continue() ->
  Children = #state.worker_pids,
  [continue(Child) || Child <- Children].

continue(Child) ->
  gen_server:cast(Child, {pause, false}).

get_element(Index, List) ->
  case lists:nth(Index, List) of
    undefined ->
      error("Index out of bounds");
    Element ->
      Element
  end.