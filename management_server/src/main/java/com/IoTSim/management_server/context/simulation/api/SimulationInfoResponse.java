package com.IoTSim.management_server.context.simulation.api;

import com.IoTSim.management_server.context.simulation.dto.SimulationProcessDto;
import com.IoTSim.management_server.context.simulation.model.Status;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

import java.util.Set;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class SimulationInfoResponse {
    @Schema(description = "Идентификатор симуляции")
    private Long id;
    @Schema(description = "Название симуляции")
    private String name;
    @Schema(description = "Статус симуляции")
    private Status simulationStatus;
    @Schema(description = "Приватность симуляции")
    private Boolean isPrivate;
    @Schema(description = "Количество устройств")
    private Long amountDevice;
    @Schema(description = "Список процессов симуляции", nullable = true)
    private Set<SimulationProcessDto> simulationProcessDtoSet;
}
