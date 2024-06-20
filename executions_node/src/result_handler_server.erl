%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(result_handler_server).

-behaviour(gen_server).
-include("../_build/default/lib/amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(result_handler_server_state, {rabbit_channel, rabbit_connection}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, Connection} = amqp_connection:start(rabbitmq_config()),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  %% Get the queue name from configuration
  Queue = application:get_env(executions_node_application, result_reporting_queue, "queue1"),
  Exchange = application:get_env(executions_node_application, result_reporting_exchange, "queue1"),
  Routing_key = application:get_env(executions_node_application, result_reporting_routing_key, "queue1"),

  %% Declare the queue
  Declare = #'queue.declare'{
    queue = Queue,
    durable = true
  },
  Binding = #'queue.bind'{
    queue = Queue,
    exchange    = Exchange,
    routing_key = Routing_key},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
  #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
  {ok, #result_handler_server_state{rabbit_channel = Channel, rabbit_connection = Connection}}.

handle_call(stop, _From, State= #result_handler_server_state{}) ->
  %% Close the channel and connection
  amqp_channel:close(State#result_handler_server_state.rabbit_channel),
  amqp_connection:close(State#result_handler_server_state.rabbit_connection);

handle_call(_Request, _From, State = #result_handler_server_state{}) ->
  {reply, ok, State}.

handle_cast({calculation, Result}, State = #result_handler_server_state{}) ->
  rabbit_send_response({calculation, Result}, State#result_handler_server_state.rabbit_channel),
  {noreply, State};

handle_cast(_Request, State = #result_handler_server_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #result_handler_server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #result_handler_server_state{}) ->
  ok.

code_change(_OldVsn, State = #result_handler_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
  Exchange = application:get_env(result_reporting_exchange),
  Routing_key = application:get_env(result_reporting_routing_key),
  ok = amqp_channel:call(Channel,
    #'basic.publish'{
      exchange = Exchange,
      routing_key = Routing_key
    },
    #amqp_msg{
      payload = Response
    }).