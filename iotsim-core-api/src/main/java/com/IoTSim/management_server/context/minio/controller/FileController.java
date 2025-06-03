package com.IoTSim.management_server.context.minio.controller;

import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.minio.config.MinioProperties;
import com.IoTSim.management_server.context.minio.service.MinioService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;

import static com.IoTSim.management_server.utilities.ImageClass.*;

@RestController
@RequiredArgsConstructor
@RequestMapping(Endpoints.MINIO)
public class FileController {
    private final MinioService minioService;
    private final MinioProperties minioProperties;

    @PostMapping(Endpoints.UPLOAD_FILE)
    public ResponseEntity<String> uploadFile(@RequestParam("file") MultipartFile file) {
        String objectName = file.getOriginalFilename();

        try (InputStream inputStream = file.getInputStream()) {
            String contentType = file.getContentType();
            minioService.uploadFile(minioProperties.getBucketName(), objectName, inputStream, contentType);
            return ResponseEntity.ok("File uploaded successfully to bucket: " + minioProperties.getBucketName());
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body("Error occurred: " + e.getMessage());
        }
    }

    @GetMapping(Endpoints.DOWNLOAD_FILE)
    public ResponseEntity<InputStreamResource> downloadFile(@PathVariable String filename) {
        try {
            InputStream fileStream = minioService.downloadFile(minioProperties.getBucketName(), filename);
            MediaType mediaType = MediaType.APPLICATION_OCTET_STREAM;

            if (filename.endsWith(JPG) || filename.endsWith(JPEG)) {
                mediaType = MediaType.IMAGE_JPEG;
            } else if (filename.endsWith(PNG)) {
                mediaType = MediaType.IMAGE_PNG;
            } else if (filename.endsWith(PDF)) {
                mediaType = MediaType.APPLICATION_PDF;
            }

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(mediaType);
            headers.setContentDispositionFormData("attachment", filename);

            return ResponseEntity.ok()
                    .headers(headers)
                    .body(new InputStreamResource(fileStream));
        } catch (Exception e) {
            return ResponseEntity.notFound().build();
        }
    }
}
