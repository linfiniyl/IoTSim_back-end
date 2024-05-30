%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sup).

-behaviour(supervisor).

-export([start_link/1, init/0]).

-record('basic.publish', {exchange, routing_key}).
-record('basic.ack', {delivery_tag}).
-record('basic.deliver', {delivery_tag}).
-record('basic.cancel_ok', {}).
-record('queue.declare_ok', {}).
-record('queue.declare', {queue}).
-record('queue.bind', {queue, exchange, routing_key}).
-record('basic.consume', {queue}).
-record('basic.consume_ok', {consumer_tag}).
-record('exchange.declare', {exchange}).
-record(amqp_params_network, {username, password, virtual_host, host, port, node, client_properties}).
-record(simulation_params, {user_id, simulation_id, entity_id, amount, arguments_of_functions, route}).
-record(amqp_msg, {payload}).
-record(rabbitmq, {queue, exchange, routing_key, queue_result, exchange_result, routing_key_result,channel}).
-record(file_info, {module_name}).

start_link(Args) ->
  {User_id, Simulation_id, Entity_id, Amount,Username, Password, Virtual_host,
    Port, Queue, Exchange, Routing_Key ,Queue_Result, Exchange_Result, Routing_Key_Result} = Args,
  #simulation_params(user_id = User_id, Simulation_id = simulation_id, Entity_id = entity_id, amount = Amount),
  #amqp_params_network(username = Username, password = Password, virtual_host = Virtual_host, port = Port),
  #rabbitmq(queue = Queue,exchange = Exchange, routing_key = Routing_Key,
    queue_result = Queue_Result, exchange_result = Exchange_Result, routing_key_result = Routing_Key_Result),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

loop(Channel) ->
  receive
  %% This is the first message received
    #'basic.consume_ok'{} ->
      loop(Channel);

  %% This is received when the subscription is cancelled
    #'basic.cancel_ok'{} ->
      ok;

  %% A delivery
    {#'basic.deliver'{delivery_tag = Tag}, Content} ->
      {func_args, {Args}, route, {Route}} = Content,
      _ = Content,

      amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
      ChildList = [supervisor:start_child(self(), [{
        #simulation_params.user_id, #simulation_params.simulation_id,
        #simulation_params.entity_id, Entity_Number,
        Args, Route,self()
      }]) || Entity_Number <- lists:seq(1, #simulation_params.amount)],
      Publish =
        #'basic.publish'{exchange = #rabbitmq.exchange_result, routing_key = #rabbitmq.routing_key_result},
      amqp_channel:cast(Channel, Publish, #amqp_msg{payload = {successful_start_threads, {amount, #simulation_params.amount}}}),

      loop(Channel);
  %% A sending data
    { calculation , Result}  -> Publish =
      #'basic.publish'{exchange = #rabbitmq.exchange_result, routing_key = #rabbitmq.routing_key_result},
      amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Result}),
      loop(Channel)
  end.

connect_to_queue(Queue, Queue_Result) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  #rabbitmq(channel = Channel),
  #'basic.consume_ok'{consumer_tag = Tag} =
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue}, self()),
  Channel.

init() ->
  Channel = connect_to_queue(#rabbitmq.queue, #rabbitmq.queue_result),
  Child = #{id => 'simulation',
    start => {module, start_link, []},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [#file_info.module_name]},
  {ok, {#{strategy => simple_one_for_one,
    intensity => 5,
    period => 30},
    [Child]}
  },
loop(Channel).
