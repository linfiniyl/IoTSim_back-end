package com.IoTSim.management_server.api.security.service;


import com.IoTSim.management_server.api.security.api.AuthenticationRequest;
import com.IoTSim.management_server.api.security.api.AuthenticationResponse;
import com.IoTSim.management_server.api.security.api.RegisterRequest;
import com.IoTSim.management_server.context.user.model.Role;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import io.jsonwebtoken.Claims;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthenticationService {

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;

    private final AuthenticationManager authenticationManager;

    @Transactional
    public AuthenticationResponse register(RegisterRequest request) {
        log.debug("Registering new user with email: {}", request.getEmail());

        User user = User.builder()
                .firstname(request.getFirstname())
                .lastname(request.getLastname())
                .email(request.getEmail())
                .password(passwordEncoder.encode(request.getPassword()))
                .isEnabled(true)
                .role(Role.USER)
                .build();

        user = userRepository.saveAndFlush(user);
        String accessToken = jwtService.generateAccessToken(user);
        String refreshToken = jwtService.generateRefreshToken(user);

        log.info("User registered successfully: {}", request.getEmail());

        return AuthenticationResponse.builder()
                .token(accessToken)
                .refreshToken(refreshToken)
                .build();
    }

    public AuthenticationResponse authenticate(AuthenticationRequest request) {
        log.debug("Authenticating user with email: {}", request.getEmail());

        authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(
                        request.getEmail(),
                        request.getPassword()
                )
        );

        User user = userRepository.findByEmail(request.getEmail()).orElseThrow();
        String accessToken = jwtService.generateAccessToken(user);
        String refreshToken = jwtService.generateRefreshToken(user);

        log.info("User authenticated successfully: {}", request.getEmail());

        return AuthenticationResponse.builder()
                .token(accessToken)
                .refreshToken(refreshToken)
                .build();
    }

    public AuthenticationResponse getAccessToken(@NonNull String refreshToken) {
        log.debug("Getting access token using refresh token");

        if (jwtService.verifyToken(refreshToken)) {
            final Claims claims = jwtService.extractAllClaims(refreshToken);
            User user = userRepository.findByEmail(claims.getSubject()).orElseThrow();

            String accessToken = jwtService.generateAccessToken(user);
            log.info("Access token generated using refresh token for user: {}", user.getEmail());

            return AuthenticationResponse.builder()
                    .token(accessToken)
                    .refreshToken(null)
                    .build();
        }

        log.warn("Invalid refresh token");
        return AuthenticationResponse.builder()
                .token(null)
                .refreshToken(null)
                .build();
    }

    public AuthenticationResponse refresh(@NonNull String refreshToken) {
        log.debug("Refreshing access token using refresh token");

        if (jwtService.verifyToken(refreshToken)) {
            final Claims claims = jwtService.extractAllClaims(refreshToken);
            User user = userRepository.findByEmail(claims.getSubject()).orElseThrow();

            String accessToken = jwtService.generateAccessToken(user);
            String newRefreshToken = jwtService.generateRefreshToken(user);

            log.info("Access token refreshed for user: {}", user.getEmail());

            return AuthenticationResponse.builder()
                    .token(accessToken)
                    .refreshToken(newRefreshToken)
                    .build();
        }

        log.warn("Invalid refresh token");
        return AuthenticationResponse.builder()
                .token(null)
                .refreshToken(null)
                .build();
    }
}
