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
public class SimulationInstanceDto {

    @Schema(description = "Идентификатор потока")
    private Long id;
    @Schema(description = "Номер устройства")
    private Long deviceNumber;
    @Schema(description = "Статус процесса симуляции")
    private Status status;
    @Schema(description = "Время выполнения процесса в секундах")
    private Long processTime;

}
