package com.IoTSim.management_server.context.attribute.service;

import com.IoTSim.management_server.api.exceptions.*;
import com.IoTSim.management_server.context.attribute.api.*;
import com.IoTSim.management_server.context.attribute.dto.AttributeDto;
import com.IoTSim.management_server.context.attribute.dto.AttributeTemplateDto;
import com.IoTSim.management_server.context.attribute.mapper.AttributeMapper;
import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeRelation;
import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;

import com.IoTSim.management_server.context.attribute.repository.AttributeAmountRepository;
import com.IoTSim.management_server.context.attribute.repository.AttributeRelationRepository;
import com.IoTSim.management_server.context.attribute.repository.AttributeTemplateRepository;
import com.IoTSim.management_server.context.attribute.utils.AttributeUtils;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.simulation.repository.SimulationRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import com.IoTSim.management_server.context.util.AccessCheckUtils;
import com.IoTSim.management_server.context.util.ContextUtils;
import com.IoTSim.management_server.context.util.RepositoryUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;


import java.util.List;
import java.util.stream.Collectors;


@Slf4j
@RequiredArgsConstructor
@Service
public class AttributeServiceImpl implements AttributeService {

    private final AttributeTemplateRepository attributeRepository;
    private final AttributeAmountRepository attributeAmountRepository;
    private final AttributeMapper mapper;
    private final AttributeRelationRepository attributeRelationRepository;

    /**
     * Сохраняет шаблон атрибута в бд.
     * Если текущий пользователь не найден, выбрасывает исключение {@link UserNotFoundException}
     * @param attributeTemplateCreateRequest dto запроса на создание шаблона атрибута
     * @return dto ответа о сохраненном шаблона атрибуте
     */
    @Transactional
    public AttributeTemplateInfoResponse createAttributeTemplate(
            AttributeTemplateCreateRequest attributeTemplateCreateRequest
    ) {
        try {
            AttributeTemplate attributeTemplate = mapper
                    .AttributeTemplateCreateRequestToAttributeTemplate(attributeTemplateCreateRequest);
            attributeTemplate.setOwner(ContextUtils.getCurrentUser());

            AttributeTemplateInfoResponse response = mapper.AttributeTemplateToAttributeTemplateInfoResponse(
                    attributeRepository.save(attributeTemplate)
            );
            log.info(AttributeUtils.ATTRIBUTE_CREATING_INFO, response);

            return response;
        } catch (Exception e) {
            log.error(AttributeUtils.ATTRIBUTE_CREATING_ERROR_MESSAGE, e.getMessage());
            throw e;
        }
    }

    /**
     * Устанавливает начальное значение для атрибута для конкретного устройства и симуляции
     * Если текущий пользователь не найден, выбрасывает исключение {@link UserNotFoundException}
     * Если у пользователя нет доступа к симуляции, атрибуту или устройству, то выбрасывает искоючение
     * {@link AccessDeniedException}. Если нет связи между устройством и атрибутом, выбрасывает исключение
     * {@link RelationDeviceException}
     * @param attributeCreateRequest dto запроса на задание значений атрибута для конкретной симуляции
     * @return Ответ об установленных значениях
     */
    @Transactional
    public AttributeInfoResponse createAttribute(
            AttributeCreateRequest attributeCreateRequest
    ){
        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeCreateRequest.getSimulationId(), Simulation.class)
                .withModel(attributeCreateRequest.getDeviceId(), Device.class)
                .withModel(attributeCreateRequest.getAttributeId(), AttributeTemplate.class)
                .check();

        AttributeAmount attributeAmount = mapper.AttributeCreateRequestToAttributeAmount(
                        attributeCreateRequest
        );

        ContextUtils.checkRelationExist(
                attributeCreateRequest.getAttributeId(),
                attributeCreateRequest.getDeviceId(),
                log
        );

        log.info("Attribute created with ID: {}", attributeAmount.getId());
        return mapper.AttributeAmountToAttributeInfoResponse(
                attributeAmountRepository.saveAndFlush(attributeAmount)
        );
    }

    /**
     * Обновление значений шаблона атрибута
     * Если текущий пользователь не найден, выбрасывает исключение {@link UserNotFoundException}
     * Если у пользователя нет доступа к атрибуту, то выбрасывает искоючение {@link AccessDeniedException}
     * @param attributeTemplateDto dto запроса на обновление шаблона атрибута
     */
    @Transactional
    public void updateTemplate (
            AttributeTemplateDto attributeTemplateDto
    ){
        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeTemplateDto.getId(), AttributeTemplate.class)
                .check();

        AttributeTemplate attributeTemplate = mapper
                .AttributeTemplateDtoToAttributeTemplate(attributeTemplateDto);

        attributeRepository.save(
                mapper.AttributeTemplateDtoToAttributeTemplate(attributeTemplateDto)
        );
        log.info("Attribute template updated with ID: {}", attributeTemplate.getId());
    }

    /**
     * Обновляет значение атрибута для конкретной симуляции
     * Если текущий пользователь не найден, выбрасывает исключение {@link UserNotFoundException}
     * Если у пользователя нет доступа к симуляции, атрибуту или устройству, то выбрасывает искоючение
     * {@link AccessDeniedException}. Если нет связи между устройством и атрибутом, выбрасывает исключение
     * {@link RelationDeviceException}
     * @param attributeDto dto запроса на обновления значения атрибута
     */
    @Transactional
    public void updateRelation(AttributeDto attributeDto) {
        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeDto.getAttributeId(), AttributeTemplate.class)
                .withModel(attributeDto.getDeviceId(), Device.class)
                .withModel(attributeDto.getSimulationId(), Simulation.class)
                .check();

        AttributeAmount attributeAmount = mapper
                .AttributeDtoToAttributeAmount(attributeDto);

        ContextUtils.checkAttributeAmountRelation(
                attributeDto.getAttributeId(),
                attributeDto.getDeviceId(),
                attributeDto.getSimulationId(),
                attributeDto.getUserId(),
                log
        );

        attributeAmountRepository.save(attributeAmount);
        log.info("Attribute relation updated with ID: {}", attributeAmount.getId());
    }

    /**
     * Удаляет шаблон атрибута по его id
     * Если текущий пользователь не найден, выбрасывает исключение {@link UserNotFoundException}
     * Если у пользователя нет доступа к атрибуту, то выбрасывает искоючение {@link AccessDeniedException}
     * @param id идентификатор шаблона атрибута
     */
    @Transactional
    public void deleteTemplateById(
            Long id
    ){
        log.info("Deleting attribute template with ID: {}", id);

        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(id, AttributeTemplate.class)
                .check();

        attributeRepository.deleteById(id);
        log.info("Attribute template deleted with ID: {}", id);
    }

    /**
     * Удаляет значения атрибута для конкретной симуляции
     * Если текущий пользователь не найден, выбрасывает исключение {@link UserNotFoundException}
     * Если у пользователя нет доступа к симуляции, атрибуту или устройству, то выбрасывает искоючение
     * {@link AccessDeniedException}. Если нет связи между устройством и атрибутом, выбрасывает исключение
     * {@link RelationDeviceException}
     * @param deviceId идентификатор устройства
     * @param attributeId идентификатор атрибута
     * @param userId идентификатор пользователя
     * @param simulationId идентификатор симуляции
     */
    @Transactional
    public void deleteRelationById(
            Long deviceId,
            Long attributeId,
            Long userId,
            Long simulationId
    ){
        log.info("Attempting to delete relation for Device ID: {}, Attribute ID: {}, User ID: {}, Simulation ID: {}",
                deviceId,
                attributeId,
                userId,
                simulationId
        );

        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeId, AttributeTemplate.class)
                .withModel(deviceId, Device.class)
                .withModel(simulationId, Simulation.class)
                .check();

        ContextUtils.checkAttributeAmountRelation(attributeId, deviceId, simulationId, userId, log);

        attributeAmountRepository.deleteById(deviceId, attributeId, userId, simulationId);
        log.info("Successfully deleted relation for Device ID: {}, Attribute ID: {}, User ID: {}, Simulation ID: {}",
                deviceId,
                attributeId,
                userId,
                simulationId
        );
    }

    /**
     * Поиск шаблона атрибута по его id
     * @param attributeTemplateId идентификатор шаблона атрибута
     * @return информация о шаблоне атрибута
     */
    @Transactional
    public AttributeTemplateInfoResponse findAttributeTemplateById(
            Long attributeTemplateId
    ){
        log.info("Finding attribute template by ID: {}", attributeTemplateId);

        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeTemplateId, AttributeTemplate.class)
                .check();

        return mapper.AttributeTemplateToAttributeTemplateInfoResponse(
                        RepositoryUtils.findObjectById(AttributeTemplate.class, attributeTemplateId)
                );
    }

    /**
     * Ищет значения атрибута для конктретной симуляции по id атрибута, устроства, симуляции и пользователя
     * @param attributeId идентификатор атрибута
     * @param deviceId идентификатор уствройства
     * @param userId идентификатор пользователя
     * @param simulationId идентификатор симуляции
     * @return информация о значениях атрибута для конкретной симуляции
     */
    @Transactional
    public AttributeInfoResponse findAttributeById(
            Long attributeId,
            Long deviceId,
            Long userId,
            Long simulationId
    ){
        log.info("Finding attribute by ID: {}, Device ID: {}, User ID: {}, Simulation ID: {}",
                attributeId,
                deviceId,
                userId,
                simulationId
        );

        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeId, AttributeTemplate.class)
                .withModel(deviceId, Device.class)
                .withModel(simulationId, Simulation.class)
                .check();

        return mapper.AttributeAmountToAttributeInfoResponse(
                attributeAmountRepository
                        .findByDeviceIdAndAttributeIdAndUserIdAndSimulationId(
                                deviceId,
                                attributeId,
                                userId,
                                simulationId
                        )
                        .orElseThrow(RelationDeviceException::new)
        );
    }

    /**
     * Ищет все доступные текущему пользователю шаблоны атрибутов
     * @return список с информацией о доступных шаблонах атрибутов
     */
    @Transactional
    public List<AttributeTemplateInfoResponse> findAllAttributesTemplateByUserId() {
        log.info("Finding all attribute templates for user");

        List<AttributeTemplate> templates = attributeRepository
                .findAllAttributesByOwnerIdAndIsPrivateFalse(
                        ContextUtils.getCurrentUser().getId()
                );
        return templates.stream()
                .map(mapper::AttributeTemplateToAttributeTemplateInfoResponse)
                .collect(Collectors.toList());
    }

    /**
     * Ищет все значения атрибутов для данной симуляции и устровства
     * @param deviceId идентификатор уствройства
     * @param userId идентификатор пользователя
     * @param simulationId идентификатор симуляции
     * @return список значений атрибутов
     */
    @Transactional
    public List<AttributeInfoResponse> findAllAttributesById(
            Long deviceId,
            Long userId,
            Long simulationId
    ){
        log.info("Finding all attributes by Device ID: {}, User ID: {}, Simulation ID: {}", deviceId, userId, simulationId);

        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(deviceId, Device.class)
                .withModel(simulationId, Simulation.class)
                .check();

        return attributeAmountRepository.findAllAttributesByIds(deviceId, userId, simulationId);
    }

    /**
     * Удаляет связь атрибута и устройства, а также все значения атрибутов в симуляциях, если они существуют
     * @param deviceId идентификатор уствройства
     * @param attributeId идентификатор атрибута
     */
    @Transactional
    public void deleteRelationWithDeviceById(
            Long deviceId,
            Long attributeId
    ){
        log.info("Attempting to delete relation with Device ID: {}, Attribute ID: {}", deviceId, attributeId);

        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeId, AttributeTemplate.class)
                .withModel(deviceId, Device.class)
                .check();

        ContextUtils.checkRelationExist(attributeId, deviceId, log);

        attributeRelationRepository.deleteAttributeRelationByAttributeIdAndDeviceId(attributeId, deviceId);
        attributeAmountRepository.deleteByDeviceIdAndAttributeId(deviceId,attributeId);
    }

    /**
     * Создает связь атрибутов и уствройства
     * @param attributeRelationCreateRequest dto запрос на создание связи
     * @return ответ о созданной связи
     */
    @Transactional
    public AttributeRelationInfoResponse createAttributeRelation(
            AttributeRelationCreateRequest attributeRelationCreateRequest
    ){
        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(attributeRelationCreateRequest.getAttributeId(), AttributeTemplate.class)
                .withModel(attributeRelationCreateRequest.getDeviceId(), Device.class)
                .check();

        ContextUtils.checkRelationExist(
                attributeRelationCreateRequest.getAttributeId(),
                attributeRelationCreateRequest.getDeviceId(),
                log
        );

        return mapper.AttributeRelationToAttributeRelationInfoResponse(
                attributeRelationRepository.saveAndFlush(
                        mapper.AttributeRelationCreateRequestToAttributeRelation(
                                attributeRelationCreateRequest
                        )
                )
        );
    }

    /**
     * Ищет все шаблоты атрибутов, связанные с этим устроством по его id
     * @param deviceId идентификатор устройства
     * @return список с информацией о шаблонах атрибутов
     */
    @Transactional
    public List<AttributeTemplateInfoResponse> findAllAttributeTemplateByDeviceId(
            Long deviceId
    ){
        AccessCheckUtils
                .AccessForUser(ContextUtils.getCurrentUser())
                .withModel(deviceId, Device.class)
                .check();

        return mapper.AttributeTemplateListToInfoResponseList(
                attributeRepository.findAllByDeviceId(deviceId)
        );
    }
}
