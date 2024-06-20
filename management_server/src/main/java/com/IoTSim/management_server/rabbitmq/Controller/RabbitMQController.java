package com.IoTSim.management_server.rabbitmq.Controller;


import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.rabbitmq.service.CommandServiceImp;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.SIMULATION_OPERATION)
public class RabbitMQController {

    private final CommandServiceImp  commandService;

    @PostMapping(Endpoints.OPERATION_START)
    public ResponseEntity<?> startSimulation(
            @RequestParam("simulationId") Long simulationId
    ){
        commandService.startSimulation(simulationId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @PostMapping(Endpoints.OPERATION_STOP)
    public ResponseEntity<?> stopSimulation(
            @RequestParam("simulationId") Long simulationId
    ){
        commandService.stopSimulation(simulationId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @PostMapping(Endpoints.OPERATION_RESTART)
    public ResponseEntity<?> restartSimulation(
            @RequestParam("simulationId") Long simulationId
    ){
        commandService.restartSimulation(simulationId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
}
