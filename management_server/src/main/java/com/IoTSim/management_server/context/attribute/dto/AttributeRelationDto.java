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
public class AttributeRelationDto {
    @Schema(description = "Идентификатор атрибута")
    private Long id;
    @Schema(description = "Идентификатор устройства")
    private Long deviceId;
}
