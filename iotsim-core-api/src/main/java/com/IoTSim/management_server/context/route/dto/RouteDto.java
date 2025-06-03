package com.IoTSim.management_server.context.route.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

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
