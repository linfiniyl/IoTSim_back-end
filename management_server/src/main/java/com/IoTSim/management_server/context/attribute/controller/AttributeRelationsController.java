package com.IoTSim.management_server.context.attribute.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.attribute.api.AttributeCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeInfoResponse;
import com.IoTSim.management_server.context.attribute.dto.AttributeDto;
import com.IoTSim.management_server.context.attribute.service.AttributeServiceImpl;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.DEVICE_RELATIONS)
public class AttributeRelationsController {

    private final AttributeServiceImpl attributeService;

    @GetMapping
    public ResponseEntity<List<AttributeInfoResponse>> getAllRelationsByDeviceId(
            @PathVariable Long deviceId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAllAttributesByDeviceId(deviceId));
    }
    @GetMapping(Endpoints.DEVICE_RELATION_ID)
    public ResponseEntity<AttributeInfoResponse> getRelationById(
            @PathVariable Long deviceId,
            @PathVariable Long attributeId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAttributeById(attributeId, deviceId));
    }
    @DeleteMapping(Endpoints.DEVICE_RELATION_ID)
    public ResponseEntity<?> deleteAttributeFromDeviceById(
            @PathVariable Long deviceId,
            @PathVariable Long attributeId
    ){
        attributeService.deleteRelationById(deviceId, attributeId);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .build();
    }

    @PutMapping(Endpoints.DEVICE_RELATION_ID)
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
