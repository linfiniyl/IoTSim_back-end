package com.IoTSim.management_server.context.attribute.api;

import com.IoTSim.management_server.context.attribute.model.AttributeType;
import com.IoTSim.management_server.context.attribute.model.SimulationTypes;
import lombok.*;

@Data
@AllArgsConstructor
@RequiredArgsConstructor
@Builder
public class AttributeCreateRequest {
    private Long id;
    @NonNull
    private Long entityId;
    @NonNull
    private String name;
    private String description = "";
    private AttributeType type = AttributeType.STRING;
    private String simulationFunction = "sin(x)";
    private SimulationTypes simulationType = SimulationTypes.SIN;
    private Long startingValue = 0L;
}
