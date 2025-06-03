package com.iotsim.erlang_module_compiler.minio.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@Slf4j
@Service
public class FileService {
    @Value("${directory}")
    private String directory;

    public void deleteIntermediateFiles(String dir) {
        try {
            Files.list(Paths.get(dir))
                    .map(Path::toFile)
                    .forEach(file -> {
                        log.debug("Deleting file '{}'", file.getName());
                        file.delete();
                    });
            log.info("All intermediate files deleted from directory '{}'", dir);
        } catch (IOException e) {
            log.error("Error occurred while deleting files in directory '{}': {}", dir, e.getMessage(), e);
            throw new RuntimeException(e);
        }
    }

    public File getFile(String filename) {
        return new File(directory + filename);
    }
}
