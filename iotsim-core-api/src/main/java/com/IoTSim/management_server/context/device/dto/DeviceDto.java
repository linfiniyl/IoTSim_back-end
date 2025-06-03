package com.IoTSim.management_server.context.device.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class DeviceDto {
    @Schema(description = "Идентификатор устройства")
    private long id;
    @Schema(description = "Название устройства")
    private String name;
    @Schema(description = "Описание устройства")
    private String description;
    @Schema(description = "Изображение устройства")
    private String picture;
    @Schema(description = "Приватность устройства")
    private Boolean isPrivate;
    @Schema(description = "Идентификатор владелеца устройства")
    private Long userId;
}
