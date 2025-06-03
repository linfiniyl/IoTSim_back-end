package com.IoTSim.management_server.context.user.api;

import com.IoTSim.management_server.context.user.model.Role;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class UserInfoResponse {
    @Schema(description = "Идентификатор пользователя")
    private Long id;
    @Schema(description = "Имя пользователя")
    private String firstname;
    @Schema(description = "Фамилия пользователя")
    private String lastname;
    @Schema(description = "Адрес электронной почты")
    private String email;
    @Schema(description = "Пароль пользователя")
    private Role role;
}
