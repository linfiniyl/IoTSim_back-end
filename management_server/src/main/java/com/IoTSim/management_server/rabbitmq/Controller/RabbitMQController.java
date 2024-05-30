package com.IoTSim.management_server.rabbitmq.Controller;


import com.IoTSim.management_server.context.simulationprocess.service.SimulationService;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/start_simulation")
public class RabbitMQController {

    @Autowired
    private RabbitTemplate rabbitTemplate;
    @Autowired
    private SimulationService simulationService;

    @PostMapping("/")
    public String startSimulation(@RequestParam("userId") Long userId, @RequestParam("simulationId") Long simulationId,
                                  @RequestParam("routingKey") String key){
        if (simulationService.findById(userId, simulationId).isPresent()){
            String message = new String(userId + " " + simulationId);
            rabbitTemplate.convertAndSend(key, message);
            return "Simulation with id - " + simulationId + " (User - " + userId + " ): Successfully Launched";
        } else {
            return "Simulation with id - " + simulationId + " (User - " + userId + " ): Not Found.";
        }
    }
}
