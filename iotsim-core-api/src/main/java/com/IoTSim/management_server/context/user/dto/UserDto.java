package com.IoTSim.management_server.context.user.dto;

import com.IoTSim.management_server.context.user.model.Role;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class UserDto {
    @Schema(description = "Идентификатор пользователя")
    private Long id;
    @Schema(description = "Имя пользователя")
    private String firstName;
    @Schema(description = "Фамилия пользователя")
    private String lastName;
    @Schema(description = "Адрес электронной почты")
    private String email;
    @Schema(description = "Пароль пользователя")
    private String password;
    @Schema(description = "Роль пользователя")
    private Role role;
}
