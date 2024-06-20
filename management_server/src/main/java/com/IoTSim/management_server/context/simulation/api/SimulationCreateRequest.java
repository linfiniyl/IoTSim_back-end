package com.IoTSim.management_server.context.simulation.api;

import com.IoTSim.management_server.context.simulation.model.Status;
import com.IoTSim.management_server.context.user.model.User;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@RequiredArgsConstructor
public class SimulationCreateRequest {
    @Schema(description = "Название симуляции")
    @NotNull
    private String name;
    @Schema(description = "Приватность симуляции")
    private Boolean isPrivate = true;
}
