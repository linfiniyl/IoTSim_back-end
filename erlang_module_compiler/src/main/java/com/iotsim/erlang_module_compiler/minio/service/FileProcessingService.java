package com.iotsim.erlang_module_compiler.minio.service;

import com.iotsim.erlang_module_compiler.Compiler.ErlangCompilerService;
import com.iotsim.erlang_module_compiler.MessageDto;
import com.iotsim.erlang_module_compiler.minio.config.MinioConfiguration;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.io.File;

import static com.iotsim.erlang_module_compiler.MessageService.*;

@Service
@RequiredArgsConstructor
public class FileProcessingService {
    private final MinioService minioService;
    private final MinioConfiguration minioConfiguration;
    private final FileService fileService;
    private final ErlangCompilerService erlangCompilerService;

    public String processFile(MessageDto message) {
        String compilationStatus = erlangCompilerService.compileErlangFile(message);
        if (compilationStatus.equals(COMPILATION_SUCCESS)) {
            File file = fileService.getFile(message.getFileName());
            if (minioService.uploadFile(minioConfiguration.getBucketName(), file)) {
                return MINIO_UPLOAD_SUCCESS;
            }
            fileService.deleteIntermediateFiles(file.getParent());
            return MINIO_ERROR;
        } else {
            return compilationStatus;
        }
    }
}
