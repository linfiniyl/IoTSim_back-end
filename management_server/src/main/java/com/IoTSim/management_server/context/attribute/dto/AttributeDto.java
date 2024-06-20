package com.IoTSim.management_server.context.attribute.dto;

import com.IoTSim.management_server.context.attribute.model.AttributeType;
import com.IoTSim.management_server.context.attribute.model.SimulationFunctions;
import com.IoTSim.management_server.context.user.model.User;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.persistence.Column;
import jakarta.persistence.Id;
import jakarta.validation.constraints.NotNull;
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
    private Long id;
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
    @Schema(description = "Идентификатор симуляции")
    private Long simulationId;
    @Schema(description = "Идентификатор пользователя")
    private Long userId;
    @Schema(description = "Начальное значение атрибута")
    private Long startingValue;
}
