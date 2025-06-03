package com.IoTSim.management_server.context.attribute.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateInfoResponse;
import com.IoTSim.management_server.context.attribute.dto.AttributeTemplateDto;
import com.IoTSim.management_server.context.attribute.service.AttributeServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.util.List;


@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.ATTRIBUTE_TEMPLATES)
public class AttributeController {

    private final AttributeServiceImpl attributeService;

    @GetMapping
    public ResponseEntity<List<AttributeTemplateInfoResponse>> getAllAttributeTemplateByUserId() {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAllAttributesTemplateByUserId());
    }

    @GetMapping(Endpoints.ATTRIBUTE_TEMPLATE_ID)
    public ResponseEntity<AttributeTemplateInfoResponse> getAttributeTemplateById(
            @PathVariable Long attributeId
    ){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(attributeService.findAttributeTemplateById(attributeId));
    }


    @PostMapping
    public ResponseEntity<?> createAttributeTemplate(
            @RequestBody AttributeTemplateCreateRequest request
    ) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(attributeService.createAttributeTemplate(request));
    }

    @DeleteMapping(Endpoints.ATTRIBUTE_TEMPLATE_ID)
    public  ResponseEntity<?> deleteAttributeTemplateById(
            @PathVariable Long attributeId
    ){
        attributeService.deleteTemplateById(attributeId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @PutMapping
    public ResponseEntity<?> updateAttributeTemplate(
            @RequestBody AttributeTemplateDto attributeTemplateDto
    ){
        attributeService.updateTemplate(attributeTemplateDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

}
