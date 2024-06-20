%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(executions_node_application).

-behaviour(application).

-export([start/0, stop/1, start/2]).

start() ->
  executions_node_application_sup:start_link().
stop(_State) ->
  ok.

start(StartType, StartArgs) ->
  executions_node_application_sup:start_link().