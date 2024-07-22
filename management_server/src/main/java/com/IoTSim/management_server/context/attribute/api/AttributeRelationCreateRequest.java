package com.IoTSim.management_server.context.attribute.api;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeRelationCreateRequest {
    @NotNull
    @Schema(description = "Идентификатор атрибута")
    private Long id;
    @NotNull
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
}
