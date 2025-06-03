package com.IoTSim.management_server.context.route.mapper;

import com.IoTSim.management_server.context.route.dto.RouteDto;
import com.IoTSim.management_server.context.route.dto.RoutePointDto;
import com.IoTSim.management_server.context.route.model.Route;
import com.IoTSim.management_server.context.route.model.RoutePoint;
import org.mapstruct.Mapper;


import java.util.List;

@Mapper(componentModel = "spring")
public interface RouteMapper {
    List<RouteDto> routeListToRouteDtoList(List<Route> routeList);

    RouteDto routeToRouteDto(Route route);
    Route routeDtoToRoute(RouteDto routeDto);

    RoutePoint routePointDtoToRoutePoint(RoutePointDto routePointDto);

    List<RoutePointDto> routePointListToRoutePointDtoList(List<RoutePoint> routePointList);
}
