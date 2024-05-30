package com.IoTSim.management_server.context.device.model;


import lombok.Data;

import java.io.Serializable;

@Data
public class DevicesAmountId implements Serializable {

    private Long simulationId;
    private Long entityId;
}
