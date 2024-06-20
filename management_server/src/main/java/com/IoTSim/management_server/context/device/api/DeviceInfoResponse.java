package com.IoTSim.management_server.context.device.api;

import com.IoTSim.management_server.context.user.model.User;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class DeviceInfoResponse {
    @Schema(description = "Идентификатор устройства")
    private Long id;
    @Schema(description = "Название устройства")
    private String name;
    @Schema(description = "Описание устройства")
    private String description;
    @Schema(description = "Изображение устройства")
    private String picture;
    @Schema(description = "Приватность устройства")
    private Boolean isPrivate;
}
