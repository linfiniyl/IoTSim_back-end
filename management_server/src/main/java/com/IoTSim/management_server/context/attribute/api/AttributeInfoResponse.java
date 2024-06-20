package com.IoTSim.management_server.context.attribute.api;

import com.IoTSim.management_server.context.attribute.model.AttributeType;
import com.IoTSim.management_server.context.attribute.model.SimulationFunctions;
import com.IoTSim.management_server.context.user.model.User;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeInfoResponse {
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
    @Schema(description = "Идентификатор атрибута")
    private Long attributeId;
    @Schema(description = "Идентификатор симуляции")
    private Long simulationId;
    @Schema(description = "Начальное значение")
    private Long startingValue;
}
