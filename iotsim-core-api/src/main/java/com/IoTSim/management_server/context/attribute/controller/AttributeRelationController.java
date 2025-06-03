package com.IoTSim.management_server.context.attribute.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.attribute.api.AttributeRelationCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateInfoResponse;
import com.IoTSim.management_server.context.attribute.service.AttributeServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.DEVICE_RELATION_ATTRIBUTES)
public class AttributeRelationController {

    private final AttributeServiceImpl attributeService;

    @PostMapping
    public ResponseEntity<?> createRelation(
            @RequestBody AttributeRelationCreateRequest request
    ){
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(attributeService.createAttributeRelation(request));
    }

    @DeleteMapping(Endpoints.DEVICE_RELATION_ATTRIBUTE_ID)
    public ResponseEntity<?> deleteAttributeFromDeviceById(
            @PathVariable Long deviceId,
            @PathVariable Long attributeId
    ){
        attributeService.deleteRelationWithDeviceById(deviceId, attributeId);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .build();
    }

    @GetMapping(Endpoints.DEVICE_RELATION_ATTRIBUTE_DEVICE_ID)
    public ResponseEntity<List<AttributeTemplateInfoResponse>> getAllRelationsByDeviceId(
            @PathVariable Long deviceId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAllAttributeTemplateByDeviceId(deviceId));
    }
}
