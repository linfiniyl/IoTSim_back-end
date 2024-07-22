package com.IoTSim.management_server.context.attribute.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class AttributeRelationId implements Serializable {
    private Long deviceId;
    private Long attributeId;
}
