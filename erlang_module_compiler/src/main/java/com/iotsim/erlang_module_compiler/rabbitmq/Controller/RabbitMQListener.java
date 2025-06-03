package com.iotsim.erlang_module_compiler.rabbitmq.Controller;

import com.iotsim.erlang_module_compiler.Compiler.ErlangCompilerService;
import com.iotsim.erlang_module_compiler.MessageDto;
import com.iotsim.erlang_module_compiler.MessageService;
import com.iotsim.erlang_module_compiler.minio.service.FileProcessingService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import static com.iotsim.erlang_module_compiler.Compiler.ErlangCompilerService.*;
import static com.iotsim.erlang_module_compiler.MessageService.*;

/**
 * Слушатель для очередей rabbitmq
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class RabbitMQListener {
    private static final String STATUS_OK = "OK";
    private static final String STATUS_ERROR = "systemError";
    private static final String STATUS_WRITING_FILE_ERROR = "writingFileError";
    private static final String STATUS_MINIO_ERROR = "minioError";

    //private final ErlangCompilerService erlangCompilerService;
    private final FileProcessingService fileProcessingService;
    private final MessageService messageService;
    private final RabbitTemplate rabbitTemplate;

    @Value("${spring.rabbitmq.routing_key}")
    private String routingKey;

    @RabbitListener(queues = "${spring.rabbitmq.queue_for_receiving}")
    public void rabbitListener(Message message){
        MessageDto dto = messageService.getMessageDto(message);
        if(log.isDebugEnabled())
            log.info("Received message {}", message.getBody());
//        switch (erlangCompilerService.compileErlangFile(dto)){
        switch (fileProcessingService.processFile(dto)){
            case MINIO_UPLOAD_SUCCESS -> rabbitTemplate.convertAndSend(
                    routingKey,
                    messageService.fillMessageHeaders(new Message(STATUS_OK.getBytes()), dto)
            );
            case MINIO_ERROR -> rabbitTemplate.convertAndSend(
                    routingKey,
                    messageService.fillMessageHeaders(new Message(STATUS_MINIO_ERROR.getBytes()), dto)
            );
//            case COMPILATION_SUCCESS -> rabbitTemplate.convertAndSend(
//                    routingKey,
//                    messageService.fillMessageHeaders(new Message(STATUS_OK.getBytes()), dto)
//            );
            case COMPILATION_FAILED -> rabbitTemplate.convertAndSend(
                    routingKey,
                    messageService.fillMessageHeaders(new Message(STATUS_ERROR.getBytes()), dto)
            );
            case WRITING_FILE_ERROR -> rabbitTemplate.convertAndSend(
                    routingKey,
                    messageService.fillMessageHeaders(new Message(STATUS_WRITING_FILE_ERROR.getBytes()), dto)
            );

        }
    }


}