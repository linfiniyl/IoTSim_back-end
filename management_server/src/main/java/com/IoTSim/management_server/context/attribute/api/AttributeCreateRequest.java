package com.IoTSim.management_server.context.attribute.api;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeCreateRequest {
    @NotNull
    @Schema(description = "Идентификатор атрибута")
    private Long id;
    @NotNull
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
    @NotNull
    @Schema(description = "Идентификатор симуляции")
    private Long simulationId;
    @NotNull
    @Schema(description = "Идентификатор пользователя")
    private Long userId;
    @NotNull
    @Schema(description = "Начальное значение атрибута")
    private Long startingValue;
}
