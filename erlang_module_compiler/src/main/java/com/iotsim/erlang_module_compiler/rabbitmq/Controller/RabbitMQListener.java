package com.iotsim.erlang_module_compiler.rabbitmq.Controller;

import com.iotsim.erlang_module_compiler.Compiler.ErlangCompilerService;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RabbitMQListener {
    @Autowired
    ErlangCompilerService erlangCompilerService;
    @Autowired
    RabbitTemplate rabbitTemplate;

    @RabbitListener(queues = "${spring.rabbitmq.queue_for_receiving}")
    public void rabbitListener(Message message){
        erlangCompilerService.setMessage(message);
        int result =  erlangCompilerService.compileErlangFile();
        if (result == 0){
            Message sendingMessage = new Message("OK".getBytes());
            sendingMessage.getMessageProperties().getHeaders().put("fileName", erlangCompilerService.getFileName());
            sendingMessage.getMessageProperties().getHeaders().put("userId", erlangCompilerService.getUserId());
            sendingMessage.getMessageProperties().getHeaders().put("simulationId", erlangCompilerService.getSimulationId());
            sendingMessage.getMessageProperties().getHeaders().put("entityId", erlangCompilerService.getEntityId());
            sendingMessage.getMessageProperties().getHeaders().put("attributeId", erlangCompilerService.getAttribute());
            sendingMessage.getMessageProperties().getHeaders().put("entityNumber", erlangCompilerService.getEntityNumber());

            rabbitTemplate.convertAndSend("${spring.rabbitmq.routing_key}", message);
        } else if (result == -1){
            System.out.println("Error. Bad Compilation");
        }
    }
}
