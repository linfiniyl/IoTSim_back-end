package com.IoTSim.management_server.config;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.info.License;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import io.swagger.v3.oas.annotations.servers.Server;
import org.springframework.context.annotation.Configuration;

@Configuration
@OpenAPIDefinition(
        info = @Info(
                contact = @Contact(
                        name = "Dmitriy",
                        email = "",
                        url = ""
                ),
                description = "IoTSim",
                title = "IoTSim",
                version = "1.0",
                license = @License(
                        name = " ",
                        url = " "
                ),
                termsOfService = ""
        ),
        servers = {
                @Server(
                        description = "local",
                        url = "http://localhost:8081"
                ),
                @Server(
                        description = "PREPROD",
                        url = "http://iotsim.cloud.sdcloud.io"
                ),
                @Server(
                        description = "PROD",
                        url = "http://iotsim.cosm-lab.science"
                )
        },
        security = {
                @SecurityRequirement(
                        name = "bearerAuth"
                )
        }
)
@SecurityScheme(
        name = "bearerAuth",
        description = "JWT Authorization",
        type = SecuritySchemeType.HTTP,
        scheme = "bearer",
        bearerFormat = "JWT"
)
public class SwaggerConfig {

}
