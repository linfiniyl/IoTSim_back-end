package com.IoTSim.management_server.context.device.api;

import com.IoTSim.management_server.context.user.model.User;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class DeviceCreateRequest {
    @Schema(description = "Название устройства")
    @NotNull
    private String name;
    @Schema(description = "Описание устройства")
    private String description = " ";
    @Schema(description = "Изображение устройства")
    private String picture = "not_picture";
    @Schema(description = "Приватность устройства")
    private Boolean isPrivate = true;
}
