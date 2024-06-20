package com.IoTSim.management_server.context.route.dto;

import com.IoTSim.management_server.context.route.model.RoutePoint;
import com.IoTSim.management_server.context.user.model.User;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

import java.util.Set;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class RouteDto {
    @Schema(description = "Идентификатор маршрута")
    private Long id;
    @Schema(description = "Название маршрута")
    private String name;
    @Schema(description = "Приватность маршрута")
    private Boolean isPrivate;
    @Schema(description = "Идентификатор владелеца маршрута")
    private Long ownerId;
}
