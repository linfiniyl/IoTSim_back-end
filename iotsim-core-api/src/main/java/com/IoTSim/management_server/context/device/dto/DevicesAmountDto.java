package com.IoTSim.management_server.context.device.dto;


import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class DevicesAmountDto {
    @Schema(description = "Идентификатор симуляции")
    private Long simulationId;
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
    @Schema(description = "Количество устройств")
    private Long amount;
}
