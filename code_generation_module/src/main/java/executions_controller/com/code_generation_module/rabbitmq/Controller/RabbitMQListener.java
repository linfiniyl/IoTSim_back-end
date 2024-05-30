package executions_controller.com.code_generation_module.rabbitmq.Controller;

import executions_controller.com.code_generation_module.Entities.EntitiesAmount;
import executions_controller.com.code_generation_module.Entities.Simulation;
import executions_controller.com.code_generation_module.Service.EntitiesAmountService;
import executions_controller.com.code_generation_module.Service.SimulationService;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;

@Component
public class RabbitMQListener{

    @Autowired
    private RabbitTemplate rabbitTemplate;
    @Autowired
    private SimulationService simulationService;
    @Autowired
    private EntitiesAmountService entitiesAmountService;

    @RabbitListener(queues = "${spring.rabbitmq.queue}")
    public void rabbitListener(Message message){
        Long userId = message.getMessageProperties().getHeader("userId");
        Long simulationId = message.getMessageProperties().getHeader("simulationId");
        Simulation simulation = simulationService.findById(userId, simulationId).get();
        List<EntitiesAmount> entitiesAmountList = entitiesAmountService.findAllById(userId, simulationId);
        HashMap<Long, Long> amount = new HashMap<>();

        for(EntitiesAmount entity : entitiesAmountList){
            amount.put(entity.getEntityId(), entity.getAmount());
        }

        System.out.println(simulationService.findById(userId,simulationId).get().getName());
    }
}
