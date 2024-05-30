%%%-------------------------------------------------------------------
%%% @author Дмитрий
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. март 2024 16:16
%%%-------------------------------------------------------------------
%%%   -module(module_name).
%%%   -author(author_name). (not required)
%%%-------------------------------------------------------------------
-module(template).
-author("Дмитрий").

%% API
-export([]).

-record(info, {user_id, simulation_id, entity_id, entity_number}).

start(User_id, Simulation_id, Entity_id, Entity_number, Points, Args, Parent) ->
  #info{user_id = User_id, simulation_id = Simulation_id, entity_id = Entity_id, entity_number = Entity_number},
  simulate(Points, [], Args, 0, Parent).

%%Calculating simulation
calculate(Args, Point, Time) ->
    %%%-------------------------------------------------------------------
    %%%
    %%% {args, {value_name_1, value_1}, {value_name_2, value_2}, {....}, {value_name_n, value_n}} = Args,
    %%%
    %%%-------------------------------------------------------------------
  {args, {speed, Speed_value}, {fuel, Fuel_value}} = Args,
  {X, Y, Z} = Point,
  { calculation ,{{simulation,{id, #info.user_id, #info.simulation_id},
    {entity, #info.entity_id}, {number_entity, #info.entity_number}},
    {point, {X, Y, Z}},
    %%%-----------------------------------------------------------------
    %%%
    %%%   {parameter, value + value * math:function(Time + random:uniform(rand:normal_s(Time)))},
    %%%    ...
    %%%
    %%%
    %%%-----------------------------------------------------------------
    {speed, Speed_value + Speed_value * math:sin(Time + random:uniform(rand:normal_s(Time)))},
    {fuel, Fuel_value + Fuel_value * math:cos(Time + random:uniform(rand:normal_s(Time)))}}}.

simulate([], _, _, Time, Parent) ->
  Parent ! {finished,{process, self()}, {time, Time}},
  ok;

simulate([Point |Route], Acc , Args, Time, Parent) ->
  Parent ! calculate(Args, Point, Time),
  simulate(Route, [Point | Acc], Args, Time + 1, Parent).

