package com.IoTSim.management_server.context.route.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.geolatte.geom.Point;
import org.geolatte.geom.Position;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class RoutePointDto {
    @Schema(description = "Идентификатор точки")
    private Long id;
    @Schema(description = "Значение гео-точки")
    private Point<Position> location;
    @Schema(description = "Маршрут")
    private Long routeId;
}
