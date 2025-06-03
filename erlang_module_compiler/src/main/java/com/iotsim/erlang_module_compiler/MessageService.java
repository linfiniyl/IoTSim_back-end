package com.iotsim.erlang_module_compiler;

import org.springframework.amqp.core.Message;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import static com.iotsim.erlang_module_compiler.StringHelper.ERLANG_FILE_EXTENSION;
import static com.iotsim.erlang_module_compiler.StringHelper.UNDERSCORE;

/**
 * Сервис для работы с сообщениями {@link org.springframework.amqp.core.Message}
 */
@Service
public class MessageService {
    public static final String FILE_NAME = "fileName";
    public static final String USER_ID = "userId";
    public static final String SIMULATION_ID = "simulationId";
    public static final String DEVICE_ID = "deviceId";
    public static final String ATTRIBUTE_ID = "attributeId";
    public static final String DEVICE_NUMBER = "deviceNumber";
    public static final String COMPILATION_SUCCESS = "compilationSuccess";
    public static final String COMPILATION_FAILED = "compilationFailed";
    public static final String WRITING_FILE_ERROR = "writingFileError";
    public static final String MINIO_UPLOAD_SUCCESS = "minioUploadSuccess";
    public static final String MINIO_ERROR = "minioError";
    public static final String DEVICE_NAME = "deviceName";
    public static final String ATTRIBUTE_NAME = "attributeName";

    @Value("${file_name}")
    private String filenamePrefix;

    /**
     * Получение POJO файла из сообщения
     * @param message Сообщение
     * @return POJO файл сообщения
     */
    public MessageDto getMessageDto(Message message) {
        return MessageDto.builder()
                .entityId(getDeviceId(message))
                .attributeId(getAttributeId(message))
                .userId(getUserId(message))
                .simulationId(getSimulationId(message))
                .entityNumber(getDeviceNumber(message))
                .fileName(getFileName(message))
                .message(message)
                .build();
    }

    /**
     * Получение название файла из сообщения
     * @param message Сообщение
     * @return название файла
     */
    public String getFileName(Message message) {
        return String.join(
                UNDERSCORE,
                filenamePrefix,
                String.valueOf(getUserId(message)),
                String.valueOf(getSimulationId(message)),
                String.valueOf(getDeviceId(message)),
                String.valueOf(getDeviceNumber(message)),
                String.valueOf(getAttributeId(message)),
                ERLANG_FILE_EXTENSION
        );
    }

    /**
     * Получение идентификатора устройства из заголовков сообщения
     */
    public Long getDeviceId(Message message){
        return message.getMessageProperties().getHeader(DEVICE_ID);
    }

    /**
     * Получение идентификатора пользователя из заголовков сообщения
     */
    public Long getUserId(Message message){
        return message.getMessageProperties().getHeader(USER_ID);
    }

    /**
     * Получение идентификатора аттрибута из заголовков сообщения
     */
    public Long getAttributeId(Message message){
        return message.getMessageProperties().getHeader(ATTRIBUTE_ID);
    }

    /**
     * Получение идентификатора симуляции из заголовков сообщения
     */
    public Long getSimulationId(Message message){
        return message.getMessageProperties().getHeader(SIMULATION_ID);
    }

    /**
     * Получение названия аттрибута из заголовков сообщения
     */
    public String getAttributeName(Message message){
        return message.getMessageProperties().getHeader(ATTRIBUTE_NAME);
    }

    /**
     * Получение название устройства из заголовков сообщения
     */
    public String getDeviceName(Message message){
        return message.getMessageProperties().getHeader(DEVICE_NAME);
    }

    /**
     * Получение номер устройства из заголовков сообщения
     */
    public Long getDeviceNumber(Message message){
        return message.getMessageProperties().getHeader(DEVICE_NUMBER);
    }

    /**
     * Заполнение заголовков сообщения из POJO класса
     * @param message сообщение
     * @param messageDto POJO класс
     * @return заполненное сообщение
     */
    public Message fillMessageHeaders(Message message, MessageDto messageDto){
        message.getMessageProperties().getHeaders().put(FILE_NAME, messageDto.getFileName());
        message.getMessageProperties().getHeaders().put(USER_ID, messageDto.getUserId());
        message.getMessageProperties().getHeaders().put(SIMULATION_ID, messageDto.getSimulationId());
        message.getMessageProperties().getHeaders().put(DEVICE_ID, messageDto.getEntityId());
        message.getMessageProperties().getHeaders().put(ATTRIBUTE_ID, messageDto.getAttributeId());
        message.getMessageProperties().getHeaders().put(DEVICE_NUMBER, messageDto.getEntityNumber());
        return message;
    }
}
