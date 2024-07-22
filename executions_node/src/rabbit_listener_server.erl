%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_listener_server).

-behaviour(gen_server).
-include("../_build/default/lib/amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {rabbit_channel, rabbit_connection,supervisor_pids = []}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  %% Connect to RabbitMQ using configuration settings
  {ok, Connection} = amqp_connection:start(rabbitmq_config()),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  %% Get the queue name from configuration
  Queue = application:get_env(executions_node_application, command_and_response_queue, "queue1"),

  %% Declare the queue
  Declare = #'queue.declare'{
    queue = Queue,
    durable = true
  },
  #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),

  %% Start consuming messages from the queue
  #'basic.consume_ok'{} =
    amqp_channel:call(Channel, #'basic.consume'{queue = Queue}),
  %% Store the channel in the server state
  {ok, #state{rabbit_channel = Channel, rabbit_connection = Connection}}.

%%%===================================================================
%%% API
%%%===================================================================

start(Args) ->
  gen_server:cast(?SERVER, {start, Args}).

pause(Pid) ->
  case lists:member(Pid, #state.supervisor_pids) of
    true ->
      gen_server:cast(?SERVER, {pause, Pid});
    false ->
      gen_server:cast(?SERVER, {supervisor_not_found, Pid})
  end.

continue(Pid) ->
  case lists:member(Pid, #state.supervisor_pids) of
    true ->
    gen_server:cast(?SERVER, {continue, Pid});
    false ->
      gen_server:cast(?SERVER, {supervisor_not_found, Pid})
  end.


stop(Pid) ->
  case lists:member(Pid, #state.supervisor_pids) of
    true ->
      gen_server:cast(?SERVER, {stop, Pid});
    false ->
      gen_server:cast(?SERVER, {supervisor_not_found, Pid})
  end.

%%%===================================================================
%%% Gen_server callbacks
%%%===================================================================

handle_cast({start, Args}, State) ->
  {args, {supervisor_id, Supervisor_id},
    _,
    _,
    _,
    _} = Args,
  %% Create a new supervisor and start the simulation
  {ok, Supervisor} = supervisor_node:start_link(Args),
  %% Install monitor on supervisor
  erlang:monitor(process, Supervisor),
  NewState = State#state{supervisor_pids = [{Supervisor,Args} | State#state.supervisor_pids]},
  rabbit_send_response(State#state.rabbit_channel,
                            {ok,
                                  {supervisor_id, Supervisor_id},
                                  {supervisor_pid, Supervisor}
                            }),
  {noreply, NewState};

handle_cast({pause, Pid}, State) ->
  case lists:member({Pid, {}}, State#state.supervisor_pids) of
    true ->
      Pid:pause(),
      rabbit_send_response(State#state.rabbit_channel, {ok, Pid});
    false ->
      rabbit_send_response(State#state.rabbit_channel, {error, not_found, Pid})
  end,
  {noreply, State};

handle_cast({continue, Pid}, State) ->
  case lists:member({Pid, {}}, State#state.supervisor_pids) of
    true ->
      Pid:continue(),
      rabbit_send_response(State#state.rabbit_channel, {ok, Pid});
    false ->
      rabbit_send_response(State#state.rabbit_channel, {error, not_found, Pid})
  end,
  {noreply, State};
handle_cast({stop, Pid}, State) ->
  case lists:member({Pid, {}}, State#state.supervisor_pids) of
    true ->
      Pid:stop(),
      NewState = State#state{supervisor_pids = lists:delete(Pid, State#state.supervisor_pids)},
      rabbit_send_response(State#state.rabbit_channel, {ok, Pid});
    false ->
      rabbit_send_response(State#state.rabbit_channel, {error, not_found, Pid}),
      NewState = State
  end,
  {noreply, NewState};
handle_cast({supervisor_not_found, Pid}, State) ->
  rabbit_send_response(State#state.rabbit_channel, {supervisor_not_found, Pid}),
  {noreply, State};

handle_cast({finished, Result}, State) ->
  rabbit_send_response({finished, Result}, State#state.rabbit_channel),
  {noreply, State};
handle_cast(_Msg, State) ->
  %% Ignore other messages
  {noreply, State}.

handle_call(stop, _From, State) ->
  %% Close the channel and connection
  amqp_channel:close(State#state.rabbit_channel),
  amqp_connection:close(State#state.rabbit_connection),

  {stop, normal, ok, State};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_info({#'basic.deliver'{}, Content}, State) ->
  %% Process the message
  handle_rabbit_message(Content),

  {noreply, State};

handle_info({'DOWN', _, process, Pid, Reason}, State) ->
  case Reason of
    normal ->
      %% Удалить супервизора из списка супервизоров
      NewPids = lists:filter(fun({P, _}) -> P /= Pid end, State#state.supervisor_pids),
      %% Отправить сообщение о завершении работы через RabbitMQ
      rabbit_send_response(State#state.rabbit_channel, {finished, {Pid}}),
      %% Обновить состояние GenServer с новым списком супервизоров
      {noreply, State#state{supervisor_pids=NewPids}};
    _ ->
      {_, Args} = lists:keyfind(fun ({X, _}) -> X end, Pid, State#state.supervisor_pids),
      %% Запустить новую копию супервизора с теми же аргументами, что и ранее
      NewPid = supervisor_node:start_link(Args),
      erlang:monitor(process, NewPid),
      NewPids = lists:map( replace_pid(Pid, NewPid), State#state.supervisor_pids),

      rabbit_send_response(State#state.rabbit_channel, {fatal_error, {Pid}}),
      %% Обновить состояние GenServer с новым PID супервизора
      {noreply, State#state{supervisor_pids=NewPids}}
  end;

handle_info({'EXIT',Pid}, State) ->
  %% Удалить супервизора из списка супервизоров
  NewPids = lists:filter(fun({P, _}) -> P /= Pid end, State#state.supervisor_pids),
  %% Отправить сообщение о завершении работы через RabbitMQ
  rabbit_send_response(State#state.rabbit_channel, {finished, {Pid}}),
  %% Обновить состояние GenServer с новым списком супервизоров
  {noreply, State#state{supervisor_pids=NewPids}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


handle_rabbit_message(Message) ->
  #amqp_msg{payload = Payload} = Message,
  case jsx:is_json(Payload) of
    true -> Json = jsx:decode(Payload, [{return_maps, true}]),
      Command = maps:get(<<"command">>, Json),
      handle_command(Command, Json);
    false ->
      io:format(Payload)
  end.

handle_command(Command, Body) ->
  case Command of
    <<"start">> ->
      SupervisorId = maps:get( <<"supervisorId">>, Body),
      UUID = maps:get(<<"UUID">>, Body),
      ArgsOfFunctions = maps:get(<<"ArgsOfFunctions">>, Body),
      Route = maps:get(<<"route">>, Body),
      ModuleName = maps:get( <<"moduleName">>, Body),
      Args ={args, {supervisor_id, SupervisorId},
        {worker_id ,UUID},
        ArgsOfFunctions,
        {route, Route},
        {module_name ,ModuleName}
      },
      start(Args);
    <<"pause">> ->
      Pid = maps:get(<<"supervisorId">>, Body),
      pause(Pid);
    <<"continue">> ->
      Pid = maps:get(<<"supervisorId">>, Body),
      continue(Pid);
    <<"stop">> ->
      Pid = maps:get(<<"supervisorId">>, Body),
      stop(Pid);
    _ ->
      {unknown_command, Command}
  end.

rabbitmq_config() ->
  %% Read RabbitMQ configuration from application environment
  #amqp_params_network{
    host = application:get_env(executions_node_application, rabbitmq_host, "localhost"),
    port = application:get_env(executions_node_application, rabbitmq_port, 5672),
    username = application:get_env(executions_node_application, rabbitmq_username, <<"guest1">>),
    password = application:get_env(executions_node_application, rabbitmq_password, <<"guest">>)
  }.

rabbit_send_response(Channel ,Response) ->
  %% Connect to RabbitMQ and send the response
  Payload = erlang:term_to_binary(Response),
  Exchange = application:get_env(executions_node_application, command_and_response_exchange, ""),
  Routing_key = application:get_env(executions_node_application, command_and_response_routing_key, ""),
  Publish =
    #'basic.publish'{
        exchange = Exchange,
        routing_key = Routing_key
    },
    amqp_channel:cast(
      Channel,
      Publish,
      #amqp_msg{
        payload = Payload
    }).
replace_pid(Pid, NewPid) ->
  fun ({X, Y}) ->
    if X == Pid -> {NewPid, Y};
      true -> {X, Y}
    end
  end.
