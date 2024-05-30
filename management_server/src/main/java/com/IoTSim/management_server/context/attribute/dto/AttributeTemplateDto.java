package com.IoTSim.management_server.context.attribute.dto;

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
public class AttributeTemplateDto {
    private Long id;
    private User owner;
    private String name;
    private String description;
    private AttributeType type;
    private String simulationFunction;
    private SimulationTypes simulationType;
}
