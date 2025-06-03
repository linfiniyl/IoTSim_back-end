package com.IoTSim.management_server.api.security.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.api.security.api.AuthenticationRequest;
import com.IoTSim.management_server.api.security.api.AuthenticationResponse;
import com.IoTSim.management_server.api.security.api.RefreshJwtRequest;
import com.IoTSim.management_server.api.security.api.RegisterRequest;
import com.IoTSim.management_server.api.security.service.AuthenticationService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(Endpoints.AUTH)
@AllArgsConstructor
public class AuthController {

    private final AuthenticationService authenticationService;

    @Operation(summary = "Регистрация пользователя")
    @PostMapping(Endpoints.REGISTER)
    public ResponseEntity<AuthenticationResponse> registerUser(
            @RequestBody @Valid RegisterRequest request
            ) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(authenticationService.register(request));
    }

    @Operation(summary = "Авторизация пользователя")
    @PostMapping(Endpoints.LOGIN)
    public ResponseEntity<AuthenticationResponse> authenticateUser(
            @RequestBody @Valid AuthenticationRequest request
            ) {
        return ResponseEntity.ok(authenticationService.authenticate(request));
    }

    @Operation(summary = "Получить новый access токен")
    @PostMapping(Endpoints.GET_ACCESS_TOKEN)
    public ResponseEntity<AuthenticationResponse> getNewAccessToken(
            @RequestBody @Valid RefreshJwtRequest request
    ) {

        return ResponseEntity.ok(authenticationService.getAccessToken(request.getRefreshToken()));
    }

    @Operation(summary = "Получить новый refresh токен")
    @PostMapping(Endpoints.REFRESH)
    public ResponseEntity<AuthenticationResponse> getNewRefreshToken(
            @RequestBody @Valid RefreshJwtRequest request
    ) {
        return ResponseEntity.ok(authenticationService.refresh(request.getRefreshToken()));
    }

}
