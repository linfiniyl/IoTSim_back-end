package executions_controller.com.code_generation_module.rabbitmq.Service;

import executions_controller.com.code_generation_module.Entities.AttributeAmount;
import executions_controller.com.code_generation_module.Service.AttributeService;
import executions_controller.com.code_generation_module.Service.EntitiesAmountService;
import executions_controller.com.code_generation_module.Service.EntityService;
import executions_controller.com.code_generation_module.Service.SimulationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class CodeGeneratorService {

    @Autowired
    private SimulationService simulationService;
    @Autowired
    private EntitiesAmountService entitiesAmountService;
    @Autowired
    private AttributeAmount attributeAmount;
    @Autowired
    private AttributeService attributeService;
    @Autowired
    private EntityService entityService;

    public String generateCode(Long userId, Long simulationId, Long entityId){
        String moduleCode = "";



        return moduleCode;
    }

    public CodeGeneratorService() {
    }

}
