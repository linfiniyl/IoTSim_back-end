package com.IoTSim.management_server.context.device.controller;

import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.attribute.service.AttributeAmountService;
import com.IoTSim.management_server.context.device.service.DeviceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;

@RestController
@RequestMapping(("/api/v1/entities"))
public class DeviceController {
    @Autowired
    private DeviceService deviceService;
    @Autowired
    private AttributeAmountService attributeAmountService;

    @GetMapping
    public List<Device> allEntities(){
        return deviceService.findAll();
    }
    @GetMapping("/{id}")
    public ResponseEntity<Optional<Device>> getEntity(@PathVariable Long id){
        try{
            Optional<Device> entity = deviceService.findById(id);
            return new ResponseEntity<>(entity, HttpStatus.OK);
        } catch (NoSuchElementException e){
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
    }
    @DeleteMapping("/{id}")
    public void deleteEntity(@PathVariable Long id){
        deviceService.deleteById(id);
    }
    @PutMapping("/{id}")
    public ResponseEntity<?> updateEntity(@RequestBody Device entity, @PathVariable Long id){
        Device entityUpdate = deviceService.update(entity, id);
        if (entityUpdate != null) {
            return new ResponseEntity<>(HttpStatus.OK);
        } else {
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
    }
    @PostMapping
    public void addEntity(@RequestBody Device entity){
        deviceService.createEntity(entity);
    }

    @GetMapping("/{id}/attributes")
    public List<AttributeAmount> allAttribute(@PathVariable Long id){
        return attributeAmountService.findAllByEntityId(id);
    }
    @DeleteMapping("/{id}/attributes/{attributeId}")
    public void deleteAttribute(@PathVariable Long id,  @PathVariable Long attributeId){
        attributeAmountService.deleteById(id, attributeId);
    }
    @PostMapping("/attributtes")
    public void addAttributeAmount(@RequestBody AttributeAmount attributeAmount){
        attributeAmountService.createAttributeAmount(attributeAmount);
    }
}
