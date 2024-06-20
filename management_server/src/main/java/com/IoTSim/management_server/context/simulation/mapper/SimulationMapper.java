package com.IoTSim.management_server.context.simulation.mapper;

import com.IoTSim.management_server.context.simulation.api.SimulationInfoResponse;
import com.IoTSim.management_server.context.simulation.dto.SimulationDto;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SimulationMapper {
    SimulationDto SimulationToSimulationDto(Simulation simulation);
    Simulation SimulationDtoToSimulation(SimulationDto simulationDto);

    SimulationInfoResponse SimulationToSimulationInfoResponse(Simulation simulation);
    List<SimulationInfoResponse> SimulationListToSimulationInfoResponseList(List<Simulation> simulationList);

}
