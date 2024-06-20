package executions_controller.com.code_generation_module.entities;


import lombok.AllArgsConstructor;
import lombok.Data;
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
