package com.IoTSim.management_server.context.attribute.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.attribute.api.AttributeCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeInfoResponse;
import com.IoTSim.management_server.context.attribute.dto.AttributeDto;
import com.IoTSim.management_server.context.attribute.service.AttributeServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.DEVICE_RELATIONS_AMOUNT)
public class AttributeAmountController {

    private final AttributeServiceImpl attributeService;

    @GetMapping(Endpoints.DEVICE_RELATION_ALL_ID_AMOUNT)
    public ResponseEntity<List<AttributeInfoResponse>> getAllRelationsByDeviceId(
            @PathVariable Long deviceId,
            @PathVariable Long userId,
            @PathVariable Long simulationId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAllAttributesById(deviceId, userId, simulationId));
    }
    @GetMapping(Endpoints.DEVICE_RELATION_ID_AMOUNT)
    public ResponseEntity<AttributeInfoResponse> getRelationById(
            @PathVariable Long deviceId,
            @PathVariable Long attributeId,
            @PathVariable Long userId,
            @PathVariable Long simulationId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAttributeById(deviceId, attributeId, userId, simulationId));
    }
    @DeleteMapping(Endpoints.DEVICE_RELATION_ID_AMOUNT)
    public ResponseEntity<?> deleteAttributeFromDeviceById(
            @PathVariable Long deviceId,
            @PathVariable Long attributeId,
            @PathVariable Long userId,
            @PathVariable Long simulationId
    ){
        attributeService.deleteRelationById(deviceId, attributeId, userId, simulationId);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .build();
    }

    @PutMapping
    public ResponseEntity<?> updateAttributeRelation(
            @RequestBody AttributeDto attributeDto
    ){
        attributeService.updateRelation(attributeDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @PostMapping
    public ResponseEntity<?> createRelation(
            @RequestBody AttributeCreateRequest request
    ){
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(attributeService.createAttribute(request));
    }
}
