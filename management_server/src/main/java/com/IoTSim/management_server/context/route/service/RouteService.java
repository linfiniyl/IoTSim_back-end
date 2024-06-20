package com.IoTSim.management_server.context.route.service;

import com.IoTSim.management_server.context.route.api.CreateRouteRequest;
import com.IoTSim.management_server.context.route.dto.RouteDto;
import com.IoTSim.management_server.context.route.dto.RoutePointDto;

import java.util.List;

public interface RouteService {
    List<RouteDto> getAllRoutesByUser();
    RouteDto getRouteById(Long routeId);
    void deleteRouteById(Long routeId);
    void updateRoute(RouteDto routeDto);
    RouteDto createRoute(CreateRouteRequest request);
    List<RoutePointDto> getAllPointByRouteId(Long routeId);
    void updatePointsByRoute(List<RoutePointDto> routePointDtoList, Long routeId);

}
