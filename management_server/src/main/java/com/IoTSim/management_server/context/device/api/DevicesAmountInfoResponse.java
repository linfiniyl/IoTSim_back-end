package com.IoTSim.management_server.context.device.api;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class DevicesAmountInfoResponse {
    @Schema(description = "Идентификатор симуляции")
    private Long simulationId;
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
    @Schema(description = "Количество устройств")
    private Long amount;
}
