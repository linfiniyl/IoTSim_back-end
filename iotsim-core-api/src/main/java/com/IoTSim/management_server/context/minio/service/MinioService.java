package com.IoTSim.management_server.context.minio.service;

import io.minio.*;
import lombok.AllArgsConstructor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;


import java.io.InputStream;

@Slf4j
@AllArgsConstructor
@Service
public class MinioService {
    private MinioClient minioClient;

    public void uploadFile(String bucketName, String objectName, InputStream inputStream, String contentType) {
        try {
            log.info("Checking if bucket '{}' exists...", bucketName);
            boolean found = minioClient.bucketExists(BucketExistsArgs.builder().bucket(bucketName).build());
            if (!found) {
                log.info("Bucket '{}' not found. Creating new bucket.", bucketName);
                minioClient.makeBucket(MakeBucketArgs.builder().bucket(bucketName).build());
            }
            log.info("Uploading file '{}' to bucket '{}'.", objectName, bucketName);
            minioClient.putObject(
                    PutObjectArgs.builder()
                            .bucket(bucketName)
                            .object(objectName)
                            .stream(inputStream, inputStream.available(), -1)
                            .contentType(contentType)
                            .build());
            log.info("File '{}' successfully uploaded to bucket '{}'.", objectName, bucketName);
        } catch (Exception e) {
            log.error("Error occurred while uploading file '{}' to bucket '{}': {}", objectName, bucketName, e.getMessage(), e);
            throw new RuntimeException("Error occurred: " + e.getMessage(), e);
        }
    }

    public InputStream downloadFile(String bucketName, String objectName) {
        try {
            log.info("Downloading file '{}' from bucket '{}'.", objectName, bucketName);
            InputStream inputStream = minioClient.getObject(
                    GetObjectArgs.builder()
                            .bucket(bucketName)
                            .object(objectName)
                            .build()
            );
            log.info("File '{}' successfully downloaded from bucket '{}'.", objectName, bucketName);
            return inputStream;
        } catch (Exception e) {
            log.error("Error occurred while downloading file '{}' from bucket '{}': {}", objectName, bucketName, e.getMessage(), e);
            throw new RuntimeException("Error occurred while downloading file: " + e.getMessage(), e);
        }
    }
}
