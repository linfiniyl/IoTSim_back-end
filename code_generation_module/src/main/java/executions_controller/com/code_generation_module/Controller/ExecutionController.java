package executions_controller.com.code_generation_module.Controller;

import executions_controller.com.code_generation_module.Entities.EntitiesAmount;
import executions_controller.com.code_generation_module.Service.EntitiesAmountService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ExecutionController {
    @Autowired
    EntitiesAmountService entitiesAmountService;

    public void startNodes(Long userId, Long simulationId){
        List<EntitiesAmount> amountEntities = entitiesAmountService.findAllById(userId, simulationId);
        Long countEntities = 0l;

        for(int i = 0; i < amountEntities.size(); i++){
            countEntities += amountEntities.get(i).getAmount();
        }



    }
}
