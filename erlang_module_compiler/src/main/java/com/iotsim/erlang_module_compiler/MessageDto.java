package com.iotsim.erlang_module_compiler;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import org.springframework.amqp.core.Message;

/**
 * POJO Класс, который хранит в себе заголовки сообщения, а также его содержание
 */
@Data
@Builder
@AllArgsConstructor
public class MessageDto {
    /**
     * Название файла
     */
    private String fileName;
    /**
     * Идентификатор устройства
     */
    private Long entityId;
    /**
     * Идентификатор аттрибута
     */
    private Long attributeId;
    /**
     * Идентификатор пользователя
     */
    private Long userId;
    /**
     * Идентификатор симуляции
     */
    private Long simulationId;
    /**
     * Номер устройства
     */
    private Long entityNumber;
    /**
     * Сообщение
     */
    private Message message;
}
