package com.iotsim.erlang_module_compiler.minio.config;

import io.minio.MinioClient;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
@Getter
public class MinioConfiguration {
    @Value("${minio.access_key}")
    private String accessKey;
    @Value("${minio.secret_key}")
    private String secretKey;
    @Value("${minio.url}")
    private String url;
    @Value("${minio.bucket_name}")
    private String bucketName;
    @Bean
    public MinioClient minioClient() {
        return MinioClient.builder()
                .endpoint(url)
                .credentials(accessKey, secretKey)
                .build();
    }

}
