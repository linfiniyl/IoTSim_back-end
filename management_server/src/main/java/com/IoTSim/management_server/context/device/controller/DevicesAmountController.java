package com.IoTSim.management_server.context.device.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.device.api.DevicesAmountCreateRequest;
import com.IoTSim.management_server.context.device.api.DevicesAmountInfoResponse;
import com.IoTSim.management_server.context.device.dto.DevicesAmountDto;
import com.IoTSim.management_server.context.device.service.DevicesAmountService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.SIMULATION_RELATIONS)
public class DevicesAmountController {
    private final DevicesAmountService devicesAmountService;

    @GetMapping(Endpoints.SIMULATION_RELATIONS_SIMULATION_ID)
    public ResponseEntity<List<DevicesAmountInfoResponse>> allRelations(
            @PathVariable Long simulationId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(devicesAmountService.findAllRelationsBySimulationId(simulationId));
    }
    @GetMapping(Endpoints.SIMULATION_RELATION_ID)
    public ResponseEntity<DevicesAmountInfoResponse> getRelation(
            @PathVariable Long simulationId,
            @PathVariable Long deviceId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(devicesAmountService.findById(simulationId, deviceId));
    }
    @DeleteMapping(Endpoints.SIMULATION_RELATION_ID)
    public ResponseEntity<?> deleteRelation(
            @PathVariable Long simulationId,
            @PathVariable Long deviceId
    ){
        devicesAmountService.deleteRelationsById(simulationId, deviceId);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .build();
    }
    @PutMapping
    public ResponseEntity<?> updateRelation(
            @RequestBody DevicesAmountDto devicesAmountDto
    ){
        devicesAmountService.updateRelation(devicesAmountDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @PostMapping
    public ResponseEntity<?>  addDevice(
            @RequestBody DevicesAmountCreateRequest request
    ){
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(devicesAmountService.createDevicesAmount(request));
    }
}
