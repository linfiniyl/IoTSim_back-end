package com.IoTSim.management_server.context.simulation.dto;

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
public class SimulationProcessDto {
    @Schema(description = "Идентификатор процесса симуляции")
    private Long id;
    @Schema(description = "Название устройства")
    private String deviceName;
    @Schema(description = "Идентификатор пользователя")
    private Long userId;
    @Schema(description = "Список потоков")
    private Set<SimulationInstanceDto> simulationInstanceDtoSet;

}
