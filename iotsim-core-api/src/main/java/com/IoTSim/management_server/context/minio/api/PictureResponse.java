package com.IoTSim.management_server.context.minio.api;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@RequiredArgsConstructor
@AllArgsConstructor
@Builder
public class PictureResponse {
    private byte[] picture;
    private String name;
    private String mediaType;
}
