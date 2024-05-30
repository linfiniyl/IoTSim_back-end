package com.IoTSim.management_server.context.attribute.model;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@NoArgsConstructor
public class AttributeAmountId implements Serializable {
    private Long entityId;

    private Long attributeId;

}
