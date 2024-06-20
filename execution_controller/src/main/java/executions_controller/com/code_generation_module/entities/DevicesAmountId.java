package executions_controller.com.code_generation_module.entities;


import lombok.Data;

import java.io.Serializable;

@Data
public class DevicesAmountId implements Serializable {

    private Long simulationId;
    private Long deviceId;
}
