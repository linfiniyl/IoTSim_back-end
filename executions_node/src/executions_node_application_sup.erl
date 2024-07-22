%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(executions_node_application_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Children = [
    %% Child specification for rabbit_listener_server
    #{
      id => rabbit_listener_server,
      start => {'rabbit_listener_server', start_link, []},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [rabbit_listener_server]
    },

    %% Child specification for result_handler_server
    #{
      id => result_handler_server,
      start => {result_handler_server, start_link, []},
      restart => permanent,
      shutdown => 1000,
      type => worker,
      modules => [result_handler_server]
    }
  ],

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    Children}
  }.
