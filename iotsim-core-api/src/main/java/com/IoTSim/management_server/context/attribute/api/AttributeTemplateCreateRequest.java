package com.IoTSim.management_server.context.attribute.api;

import com.IoTSim.management_server.context.attribute.model.AttributeType;
import com.IoTSim.management_server.context.attribute.model.SimulationFunctions;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeTemplateCreateRequest {

    @NotNull
    @Schema(description = "Название шаблона атрибута")
    private String name;
    @Schema(description = "Описание шаблона атрибута")
    private String description = "";
    @Schema(description = "Тип атрибута")
    private AttributeType type = AttributeType.STRING;
    @Schema(description = "Функция симуляции")
    private SimulationFunctions simulationFunctions = SimulationFunctions.SIN;
    @Schema(description = "Приватность шаблона атрибута")
    private Boolean isPrivate = true;
}
