package com.IoTSim.management_server.api.security.api;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.Data;

@Data
@Schema(description = "Запрос на регистрацию пользователя")
public class RegisterRequest {

    @Schema(description = "Имя пользователя", example = "Ivan")
    @Size(min = 1, max = 25, message = "Имя пользователя должно содержать от 1 до 25 символов")
    @NotBlank(message = "Имя пользователя не должно быть пустым")
    private String firstname;
    @Schema(description = "Фамилия пользователя", example = "Ivanov")
    @Size(min = 1, max = 25, message = "Фамилия пользователя должна содержать от 1 до 25 символов")
    @NotBlank(message = "Фамилия пользователя не должна быть пустым")
    private String lastname;
    @Schema(description = "Адрес электронной почты", example = "ivanov.ivan@gmail.com")
    @Size(min = 5, max = 50, message = "Адрес электронной почты должен содержать от 5 до 50 символов")
    @NotBlank(message = "Адрес электронной почты не должен быть пустым")
    @Email(message = "Email адрес должен быть в формате user@example.com")
    private String email;
    @Schema(description = "Пароль")
    @Size(min = 8,max = 32, message = "Длина пароля должна быть от 8 до 32 символов")
    private String password;
}
