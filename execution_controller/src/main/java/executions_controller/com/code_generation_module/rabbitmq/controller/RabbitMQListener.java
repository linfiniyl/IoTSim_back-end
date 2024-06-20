package executions_controller.com.code_generation_module.rabbitmq.controller;

import executions_controller.com.code_generation_module.entities.DevicesAmount;
import executions_controller.com.code_generation_module.exceptions.SimulationBuildException;
import executions_controller.com.code_generation_module.rabbitmq.service.CodeGeneratorService;
import executions_controller.com.code_generation_module.repository.DevicesAmountRepository;
import executions_controller.com.code_generation_module.repository.SimulationProcessRepository;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;

import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.List;

@Component
@RequiredArgsConstructor
public class RabbitMQListener{

    private final RabbitTemplate rabbitTemplate;
    private final CodeGeneratorService codeGeneratorService;
    @Value("${spring.rabbitmq.routing_key.execution_controller}")
    private String routingKey;
    @Value("${spring.rabbitmq.routing_key.execution_nodes}")
    private String enRoutingKey;
    private final DevicesAmountRepository devicesAmountRepository;
    private final SimulationProcessRepository simulationProcessRepository;

    @RabbitListener(queues = "${spring.rabbitmq.queue.management_server}")
    public void fromManagementServer(Message message) throws UnsupportedEncodingException {
        if(Arrays.equals(message.getBody(), "Start simulation".getBytes())) {
            Long simulationId = message.getMessageProperties().getHeader("simulationId");
            Long userId = message.getMessageProperties().getHeader("userId");
            List<DevicesAmount> devicesAmount = devicesAmountRepository
                    .findAllDevicesById(simulationId);
            for (DevicesAmount amount : devicesAmount) {
                String moduleName =
                        amount.getDevice().getName() +
                        "_" +
                        amount.getDevice().getId() +
                        "_" +
                        simulationId +
                        "_" +
                        userId;

                Message generatedCode = new Message(
                        codeGeneratorService
                                .generateCode(
                                        simulationId,
                                        userId,
                                        amount.getDeviceId(),
                                        moduleName
                                )
                                .getBytes()
                );
                Long supervisorId = simulationProcessRepository.findBySimulationIdAndUserIdAndDeviceId(
                  userId,
                  simulationId,
                  amount.getDeviceId()
                ).orElseThrow(SimulationBuildException::new).getId();
                generatedCode.getMessageProperties().getHeaders().put("supervisorId", supervisorId);
                generatedCode.getMessageProperties().getHeaders().put("moduleName", moduleName);
                generatedCode.getMessageProperties().getHeaders().put("command", "compile erlang file");
                rabbitTemplate.convertAndSend(routingKey, generatedCode);
            }

        }
    }

    @RabbitListener(queues = "${spring.rabbitmq.queue.compilation_module}")
    public void fromCompilationErlangCodeModule(Message message){
        if (Arrays.equals(message.getBody(), "successful".getBytes())) {
            Long supervisorId = message.getMessageProperties().getHeader("supervisorId");
            String moduleName = message.getMessageProperties().getHeader("moduleName");
            Message commandToNode = new Message(
                    codeGeneratorService.generateArguments(supervisorId, moduleName).getBytes()
            );

            rabbitTemplate.convertAndSend(enRoutingKey, commandToNode);
        }
    }
    @RabbitListener(queues = "${spring.rabbitmq.queue.execution_nodes}")
    public void fromExecutionNodes(Message message) throws UnsupportedEncodingException {
        System.out.println(new String(message.getBody(), "UTF-8"));
    }

}
