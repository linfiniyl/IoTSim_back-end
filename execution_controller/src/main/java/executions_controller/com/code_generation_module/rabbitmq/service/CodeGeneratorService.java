package executions_controller.com.code_generation_module.rabbitmq.service;

import executions_controller.com.code_generation_module.entities.*;
import executions_controller.com.code_generation_module.repository.*;
import executions_controller.com.code_generation_module.service.*;
import lombok.RequiredArgsConstructor;
import org.json.JSONObject;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
public class CodeGeneratorService {

    private final AttributeService attributeAmountService;
    private final DeviceRepository deviceRepository;
    private final RoutePointRepository routePointRepository;

    private final SimulationInstanceRepository simulationInstanceRepository;
    private final SimulationProcessRepository simulationProcessRepository;
    private final AttributeAmountRepository attributeAmountRepository;

    public String generateCode(Long simulationId, Long userId, Long deviceId, String moduleName){


        StringBuilder builder = new StringBuilder("""
                -module(""");
        builder.append(moduleName);

        builder.append("""
                ).

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
                        result_handler_server ! {finished,{process, self()},{uuid, #worker_state.uuid}, {time, Time}},
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
                       
                       {args,\s
                         """);

            List<AttributeAmount> amountList = attributeAmountService
                    .findAllAttributesByDeviceId(deviceId, simulationId, userId);
            for (AttributeAmount am: amountList) {
                builder.append(", {")
                        .append(am.getAttributeTemplate().getName())
                        .append(", ")
                        .append(am.getAttributeTemplate().getName())
                        .append("_value")
                        .append("}");
            }

        builder.append("""
                } = Args,
                  {calculation, {simulation,{uuid, #worker_state.uuid}},
                                      {point, Point},""");
            for (AttributeAmount am: amountList) {
                builder.append(", {")
                        .append(am.getAttributeTemplate().getName())
                        .append(", ")
                        .append(am.getAttributeTemplate().getName())
                        .append("_value")
                        .append(" + ")
                        .append(am.getAttributeTemplate().getName())
                        .append("_value")
                        .append(" * ")
                        .append("math::")
                        .append(am.getAttributeTemplate().getSimulationFunctions().toString().toLowerCase())
                        .append("(Time + rand:uniform(rand:normal_s(Time)))}.");
            }

        return builder.toString();
    }

    public JSONObject generateArguments(Long supervisorId, String moduleName){
        SimulationProcess process = simulationProcessRepository.findById(supervisorId)
                .orElseThrow(RuntimeException::new);
        List<AttributeAmount> amounts = attributeAmountRepository
                .findBySupervisorId(supervisorId);
        JSONObject json = new JSONObject();
        json.put("command", "start");
        json.put("supervisorId", supervisorId);
        json.put("UUID", Arrays.toString(simulationInstanceRepository
                .findBySimulationProcessId(process.getId())
                .stream()
                .map(SimulationInstance::getId).distinct().toArray()));
        json.put("ArgsOfFunctions",  amounts.stream()
                .map(AttributeAmount::getStartingValue)
                .collect(Collectors.toList()));
        json.put("route", Arrays.toString(routePointRepository
                .findByRouteSimulationsId(process.getSimulationId()).toArray()));
        json.put("moduleName", moduleName);

        return json;
    }
}
