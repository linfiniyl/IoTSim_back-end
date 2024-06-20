package com.IoTSim.management_server.context.device.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.device.api.DeviceCreateRequest;
import com.IoTSim.management_server.context.device.api.DeviceInfoResponse;
import com.IoTSim.management_server.context.device.dto.DeviceDto;
import com.IoTSim.management_server.context.device.service.DeviceService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.DEVICES)
public class DeviceController {
    private final DeviceService deviceService;


    @GetMapping
    public ResponseEntity<List<DeviceInfoResponse>> allDevices(){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(deviceService.findAllDevices());
    }
    @GetMapping(Endpoints.DEVICES_ID)
    public ResponseEntity<DeviceInfoResponse> getDevice(
            @PathVariable Long deviceId
    ){
       return ResponseEntity
               .status(HttpStatus.OK)
               .body(deviceService.findById(deviceId));
    }
    @DeleteMapping(Endpoints.DEVICES_ID)
    public ResponseEntity<?> deleteDevice(
            @PathVariable Long deviceId
    ){
        deviceService.deleteById(deviceId);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .build();
    }
    @PutMapping(Endpoints.DEVICES_ID)
    public ResponseEntity<?> updateDevice(
            @RequestBody DeviceDto deviceDto
    ){
        deviceService.updateDevice(deviceDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @PostMapping
    public ResponseEntity<?>  addDevice(
            @RequestBody DeviceCreateRequest request
    ){
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(deviceService.createDevice(request));
    }
}
