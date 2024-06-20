package com.IoTSim.management_server.context.simulation.dto;

import com.IoTSim.management_server.context.simulation.model.Status;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class SimulationDto {
    @Schema(description = "Идентификатор симуляции")
    private Long id;
    @Schema(description = "Название симуляции")
    private String name;
    @Schema(description = "Статус симуляции")
    private Status simulationStatus;
    @Schema(description = "Приватность симуляции")
    private Boolean isPrivate;
    @Schema(description = "Идентификатор владелеца симуляции")
    private Long userId;
}
