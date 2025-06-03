package com.IoTSim.management_server.context.device.api;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class DevicesAmountCreateRequest {
    @Schema(description = "Идентификатор симуляции")
    @NotNull
    private Long simulationId;
    @Schema(description = "Идентификатор устройства")
    @NotNull
    private Long deviceId;
    @Schema(description = "Количество устройств")
    @NotNull
    private Long amount;
}
