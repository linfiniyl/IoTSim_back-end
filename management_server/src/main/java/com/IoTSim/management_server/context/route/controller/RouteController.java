package com.IoTSim.management_server.context.route.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.route.api.CreateRouteRequest;
import com.IoTSim.management_server.context.route.dto.RouteDto;
import com.IoTSim.management_server.context.route.dto.RoutePointDto;
import com.IoTSim.management_server.context.route.service.RouteServiceImp;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(Endpoints.ROUTES)
public class RouteController {

    private final RouteServiceImp routeService;

    @GetMapping
    public ResponseEntity<List<RouteDto>> getAllRoute(){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(routeService.getAllRoutesByUser());
    }

    @GetMapping(Endpoints.ROUTE_ID)
    public ResponseEntity<RouteDto> getRouteById(
            @PathVariable Long routeId
    ) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(routeService.getRouteById(routeId));
    }

    @PostMapping
    public ResponseEntity<RouteDto> createRoute(
            @RequestBody CreateRouteRequest request
    ) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(routeService.createRoute(request));
    }
    @PutMapping
    public ResponseEntity<?> updateRoute(
            @RequestBody RouteDto routeDto
    ) {
        routeService.updateRoute(routeDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @DeleteMapping(Endpoints.ROUTE_ID)
    public ResponseEntity<?> deleteRouteById(
            @PathVariable Long routeId
    ) {
        routeService.deleteRouteById(routeId);
        return ResponseEntity
                .status(HttpStatus.NO_CONTENT)
                .build();
    }
    @PutMapping(Endpoints.ROUTE_POINT_ID)
    public ResponseEntity<?> updateRoutePoints(
            @PathVariable Long routeId,
            @RequestBody List<RoutePointDto> pointsDto
    ) {
        routeService.updatePointsByRoute(pointsDto, routeId);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
    @GetMapping(Endpoints.ROUTE_POINT_ID)
    public ResponseEntity<List<RoutePointDto>> getAllPointsByRouteId(
            @PathVariable Long routeId
    ) {
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(routeService.getAllPointByRouteId(routeId));
    }
}
