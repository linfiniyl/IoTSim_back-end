package com.IoTSim.management_server.context.attribute.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.attribute.api.AttributeCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeInfoResponse;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateInfoResponse;
import com.IoTSim.management_server.context.attribute.service.AttributeServiceImpl;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.nio.file.AccessDeniedException;
import java.util.List;


@RequiredArgsConstructor
@AllArgsConstructor
@RestController
public class AttributeController {

    private final AttributeServiceImpl attributeService;

    @GetMapping(Endpoints.ALL_USERS_ATTRIBUTE_TEMPLATE)
    public ResponseEntity<List<AttributeTemplateInfoResponse>> getAllAttributeTemplateByUserId(
            @PathVariable Long userId
    ) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAllAttributesTemplateByUserId(userId));
    }

    @GetMapping(Endpoints.ALL_ENTITY_ATTRIBUTES)
    public ResponseEntity<List<AttributeInfoResponse>> getAllAttributesByEntityId(
            @PathVariable Long entityId
    ) throws AccessDeniedException {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAllAttributesByEntityId(entityId));
    }

    @GetMapping(Endpoints.ATTRIBUTE_TEMPLATE)
    public ResponseEntity<AttributeTemplateInfoResponse> getAttributeTemplateById(
            @PathVariable Long attributeId
    ) throws AccessDeniedException {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAttributeTemplateById(attributeId));
    }
    @GetMapping(Endpoints.ATTRIBUTE)
    public ResponseEntity<AttributeInfoResponse> getAttributeById(
            @PathVariable Long entityId,
            @PathVariable Long attributeId
    ) throws AccessDeniedException {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAttributeById(attributeId, entityId));
    }
    @PostMapping(Endpoints.ATTRIBUTE)
    public ResponseEntity<?> createAttribute(
            @RequestBody AttributeCreateRequest request
    ) throws AccessDeniedException {
        attributeService.createAttribute(request);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @PostMapping(Endpoints.ATTRIBUTE_TEMPLATE)
    public ResponseEntity<?> createAttributeTemplate(
            @RequestBody AttributeTemplateCreateRequest request
    ) {
        attributeService.createAttributeTemplate(request);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @DeleteMapping(Endpoints.ATTRIBUTE)
    public ResponseEntity<?> deleteAttributeFromEntityById(
            @PathVariable Long entityId,
            @PathVariable Long attributeId
    ) throws AccessDeniedException {
        attributeService.deleteRelationById(entityId, attributeId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @DeleteMapping(Endpoints.ATTRIBUTE_TEMPLATE)
    public  ResponseEntity<?> deleteAttributeTemplateById(
            @PathVariable Long attributeId
    ) throws AccessDeniedException {
        attributeService.deleteTemplateById(attributeId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @PutMapping(E)

}
