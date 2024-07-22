package executions_controller.com.code_generation_module.rabbitmq.controller;

import executions_controller.com.code_generation_module.entities.DevicesAmount;
import executions_controller.com.code_generation_module.entities.SimulationProcess;
import executions_controller.com.code_generation_module.exceptions.SimulationBuildException;
import executions_controller.com.code_generation_module.rabbitmq.service.CodeGeneratorService;
import executions_controller.com.code_generation_module.repository.DevicesAmountRepository;
import executions_controller.com.code_generation_module.repository.SimulationProcessRepository;
import lombok.RequiredArgsConstructor;

import org.json.JSONObject;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.UnsupportedEncodingException;
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
    public void fromManagementServer(Message message) {
        JSONObject receivedMessage = new JSONObject(new String(message.getBody()));
        switch (receivedMessage.get("command").toString()) {
            case "Start simulation" -> {
                Long simulationId = (Long) receivedMessage.get("simulationId");
                Long userId = (Long) receivedMessage.get("userId");
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

                    String generatedCode =
                            codeGeneratorService
                                    .generateCode(
                                            simulationId,
                                            userId,
                                            amount.getDeviceId(),
                                            moduleName
                                    );
                    Long supervisorId = simulationProcessRepository.findBySimulationIdAndUserIdAndDeviceId(
                            userId,
                            simulationId,
                            amount.getDeviceId()
                    ).orElseThrow(SimulationBuildException::new).getId();

                    JSONObject sendingMessage = new JSONObject();
                    sendingMessage.put("command", "compile erlang file");
                    sendingMessage.put("supervisorId", supervisorId);
                    sendingMessage.put("moduleName", moduleName);
                    sendingMessage.put("erlangCode", generatedCode);

                    Message msg = new Message(sendingMessage.toString().getBytes());
                    msg.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

                    rabbitTemplate.convertAndSend(routingKey, msg);
                }

            }
            case "Stop simulation" -> {
                Long simulationId = (Long) receivedMessage.get("simulationId");
                Long userId = (Long) receivedMessage.get("userId");
                List<SimulationProcess> processes = simulationProcessRepository
                        .findBySimulationIdAndUserId(
                            simulationId,
                            userId
                        );
                for (var process : processes){
                    JSONObject sendingMessage = new JSONObject();
                    sendingMessage.put("command", "stop");
                    sendingMessage.put("supervisorId", process.getSupervisorPID());
                    Message msg = new Message(sendingMessage.toString().getBytes());
                    msg.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

                    rabbitTemplate.convertAndSend(enRoutingKey, msg);
                }

            }
            /*case "Restart simulation" -> {
                Long simulationId = (Long) receivedMessage.get("simulationId");
                Long userId = (Long) receivedMessage.get("userId");
            }*/
            case "Pause simulation" -> {
                Long simulationId = (Long) receivedMessage.get("simulationId");
                Long userId = (Long) receivedMessage.get("userId");
                List<SimulationProcess> processes = simulationProcessRepository
                        .findBySimulationIdAndUserId(
                                simulationId,
                                userId
                        );
                for (var process : processes){
                    JSONObject sendingMessage = new JSONObject();
                    sendingMessage.put("command", "pause");
                    sendingMessage.put("supervisorId", process.getSupervisorPID());
                    Message msg = new Message(sendingMessage.toString().getBytes());
                    msg.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

                    rabbitTemplate.convertAndSend(enRoutingKey, msg);
                }
            }
            case "Continue simulation" -> {
                Long simulationId = (Long) receivedMessage.get("simulationId");
                Long userId = (Long) receivedMessage.get("userId");
                List<SimulationProcess> processes = simulationProcessRepository
                        .findBySimulationIdAndUserId(
                                simulationId,
                                userId
                        );
                for (var process : processes){
                    JSONObject sendingMessage = new JSONObject();
                    sendingMessage.put("command", "continue");
                    sendingMessage.put("supervisorId", process.getSupervisorPID());
                    Message msg = new Message(sendingMessage.toString().getBytes());
                    msg.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

                    rabbitTemplate.convertAndSend(enRoutingKey, msg);
                }
            }
        }
    }

    @RabbitListener(queues = "${spring.rabbitmq.queue.compilation_module}")
    public void fromCompilationErlangCodeModule(Message message){
        JSONObject receivedMessage = new JSONObject(new String(message.getBody()));
        if ("successful".equals(receivedMessage.get("status").toString())) {
            Long supervisorId = (Long) receivedMessage.get("supervisorId");
            String moduleName = (String) receivedMessage.get("moduleName");
            Message commandToNode = new Message(
                    codeGeneratorService.generateArguments(supervisorId, moduleName)
                            .toString()
                            .getBytes()
            );
            commandToNode.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);
            rabbitTemplate.convertAndSend(enRoutingKey, commandToNode);
        }
    }
    @RabbitListener(queues = "${spring.rabbitmq.queue.execution_nodes}")
    public void fromExecutionNodes(Message message) {
        JSONObject receivedMessage = new JSONObject(new String(message.getBody()));
        /*
        В процессе
         */
    }

}
/*
JSONObject sendingMessage = new JSONObject();

                int[] pids = new int[50];
                int[] route = new int[50];
                Arrays.fill(pids, 0);
                Arrays.fill(route, 1);
       sendingMessage.put("supervisorId", "123");
                sendingMessage.put("UUID", pids);
                sendingMessage.put("ArgsOfFunctions", new int[]{1, 2});
                sendingMessage.put("route", route);
                sendingMessage.put("moduleName", "worker");
                sendingMessage.put("command", "start");
                Message msg = new Message(sendingMessage.toString().getBytes());
                msg.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);
 */