package com.IoTSim.management_server.context.minio.config;

import io.minio.MinioClient;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class MinioConfiguration {

    private final MinioProperties minioProperties;
/*
    @Bean
    public MinioClient minioClient(){
        return MinioClient
                .builder()
                .credentials(minioProperties.getAccessKey(),minioProperties.getSecretKey())
                .endpoint(minioProperties.getUrl())
                .build();
    }*/
}
