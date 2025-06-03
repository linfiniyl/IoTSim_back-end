package com.IoTSim.management_server.api.security.api;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.Data;

@Data
@Schema(description = "Запрос на аутентификацию")
public class AuthenticationRequest {

    @Schema(description = "Адрес электронной почты", example = "ivanov.ivan@gmail.com")
    @Size(min = 5, max = 50, message = "Адрес электронной почты должен содержать от 5 до 50 символов")
    @NotBlank(message = "Адрес электронной почты не должен быть пустым")
    @Email(message = "Email адрес должен быть в формате user@example.com")
    private String email;
    @Schema(description = "Пароль")
    @Size(min = 8,max = 32, message = "Длина пароля должна быть от 8 до 32 символов")
    private String password;
}
