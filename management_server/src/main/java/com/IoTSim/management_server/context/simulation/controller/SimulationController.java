package com.IoTSim.management_server.context.simulation.controller;


import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.simulation.api.SimulationCreateRequest;
import com.IoTSim.management_server.context.simulation.api.SimulationInfoResponse;
import com.IoTSim.management_server.context.simulation.dto.SimulationDto;
import com.IoTSim.management_server.context.simulation.service.SimulationService;
import lombok.RequiredArgsConstructor;
import com.IoTSim.management_server.context.device.model.DevicesAmount;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;



@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.SIMULATIONS)
public class SimulationController {

    private final SimulationService simulationService;

    @GetMapping
    public ResponseEntity<List<SimulationInfoResponse>> allSimulations(){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(simulationService.findAllAvailableSimulations());
    }

    @GetMapping(Endpoints.SIMULATION_ID)
    public ResponseEntity<SimulationInfoResponse> getSimulation(
            @PathVariable Long id
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(simulationService.findById(id));
    }

    @DeleteMapping(Endpoints.SIMULATION_ID)
    public ResponseEntity<?> deleteSimulation(
            @PathVariable Long id
    ){
        simulationService.deleteSimulationById(id);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @PostMapping
    public ResponseEntity<?> createSimulation(
            @RequestBody SimulationCreateRequest request
    ){
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(simulationService.createSimulation(request));
    }

    @PutMapping
    public ResponseEntity<?> updateSimulation(
            @RequestBody SimulationDto simulationDto
    ){
        simulationService.updateSimulation(simulationDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

}
