package com.IoTSim.management_server.context.attribute.model;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
public class AttributeAmountId implements Serializable {
    private Long deviceId;

    private Long attributeId;

    private Long simulationId;

    private Long userId;

}
