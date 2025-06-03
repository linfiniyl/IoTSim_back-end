package com.IoTSim.management_server.context.attribute.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeDto {
    @Schema(description = "Идентификатор атрибута")
    private Long attributeId;
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
    @Schema(description = "Идентификатор симуляции")
    private Long simulationId;
    @Schema(description = "Идентификатор пользователя")
    private Long userId;
    @Schema(description = "Начальное значение атрибута")
    private Long startingValue;
}
