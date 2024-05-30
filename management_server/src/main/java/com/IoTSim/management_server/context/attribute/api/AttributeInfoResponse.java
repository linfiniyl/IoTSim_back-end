package com.IoTSim.management_server.context.attribute.api;

import com.IoTSim.management_server.context.attribute.model.AttributeType;
import com.IoTSim.management_server.context.attribute.model.SimulationTypes;
import com.IoTSim.management_server.context.user.model.User;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeInfoResponse {

    private Long id;
    private Long entityId;
    private String name;
    private String description;
    private AttributeType type;
    private String simulationFunction;
    private SimulationTypes simulationType;
    private Long startingValue;
    private User owner;
}