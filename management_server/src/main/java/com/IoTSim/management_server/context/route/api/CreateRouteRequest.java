package com.IoTSim.management_server.context.route.api;

import com.IoTSim.management_server.context.route.model.RoutePoint;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

import java.util.Set;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class CreateRouteRequest {
    @NotNull
    @Schema(description = "Название маршрута")
    private String name;
    @NotNull
    @Schema(description = "Список гео-точек маршрута")
    private Set<RoutePoint> routePointSet;
    @Schema(description = "Приватность маршрута")
    private Boolean isPrivate = true;
}
