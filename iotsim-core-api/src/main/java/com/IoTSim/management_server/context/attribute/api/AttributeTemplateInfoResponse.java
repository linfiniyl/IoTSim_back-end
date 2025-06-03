package com.IoTSim.management_server.context.attribute.api;

import com.IoTSim.management_server.context.attribute.model.AttributeType;
import com.IoTSim.management_server.context.attribute.model.SimulationFunctions;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeTemplateInfoResponse {
    @Schema(description = "Идентификатор атрибута")
    private Long id;
    @Schema(description = "Название шаблона атрибута")
    private String name;
    @Schema(description = "Описание шаблона атрибута")
    private String description;
    @Schema(description = "Тип атрибута")
    private AttributeType type;
    @Schema(description = "Функция симуляции")
    private SimulationFunctions simulationFunction;
    @Schema(description = "Идентификатор владельца")
    private Long ownerId;
    @Schema(description = "Приватность шаблона атрибута")
    private Boolean isPrivate;
}
