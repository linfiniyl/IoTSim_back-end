package com.iotsim.erlang_module_compiler.minio.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import io.minio.*;

import java.io.File;
import java.io.FileInputStream;

@Slf4j
@Service
@RequiredArgsConstructor
public class MinioService {
    private final MinioClient minioClient;
    private static final String CONTENT_TYPE = "application/octet-stream";

    public boolean uploadFile(String bucketName, File file) {
        try {
            log.info("Checking if bucket '{}' exists...", bucketName);
            boolean found = minioClient.bucketExists(BucketExistsArgs.builder().bucket(bucketName).build());
            if (!found) {
                log.info("Bucket '{}' not found. Creating new bucket.", bucketName);
                minioClient.makeBucket(MakeBucketArgs.builder().bucket(bucketName).build());
            }
            log.info("Uploading file '{}' to bucket '{}'.", file.getName(), bucketName);
            minioClient.putObject(
                    PutObjectArgs.builder()
                            .bucket(bucketName)
                            .object(file.getName())
                            .stream(new FileInputStream(file), file.length(), -1)
                            .contentType(CONTENT_TYPE)
                            .build());
            log.info("File '{}' successfully uploaded to bucket '{}'.", file.getName(), bucketName);
        } catch (Exception e) {
            log.error("Error occurred while uploading file '{}' to bucket '{}': {}", file.getName(), bucketName, e.getMessage(), e);
            return false;
        }
        return true;
    }
}
