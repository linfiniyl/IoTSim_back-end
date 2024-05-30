package com.IoTSim.management_server.context.attribute.service;

import com.IoTSim.management_server.api.exceptions.AttributeNotFoundException;
import com.IoTSim.management_server.api.exceptions.EntityNotFoundException;
import com.IoTSim.management_server.api.exceptions.RelationEntityException;
import com.IoTSim.management_server.api.exceptions.UserNotFoundException;
import com.IoTSim.management_server.context.attribute.api.AttributeCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeInfoResponse;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateInfoResponse;
import com.IoTSim.management_server.context.attribute.dto.AttributeDto;
import com.IoTSim.management_server.context.attribute.dto.AttributeTemplateDto;
import com.IoTSim.management_server.context.attribute.mapper.AttributeMapper;
import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;

import com.IoTSim.management_server.context.attribute.repository.AttributeAmountRepository;
import com.IoTSim.management_server.context.attribute.repository.AttributeTemplateRepository;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@RequiredArgsConstructor
@Service
public class AttributeServiceImpl implements AttributeService {

    private final AttributeTemplateRepository attributeRepository;
    private final AttributeAmountRepository attributeAmountRepository;
    private final UserRepository userRepository;
    private final AttributeMapper mapper;
    private final DeviceRepository deviceRepository;

    @Transactional
    public void createAttributeTemplate(
            AttributeTemplateCreateRequest attributeTemplateCreateRequest
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        AttributeTemplate attributeTemplate = mapper
                .AttributeTemplateCreateRequestToAttributeTemplate(attributeTemplateCreateRequest);
        attributeTemplate.setOwner(user);
       attributeRepository.save(attributeTemplate);
    }

    @Transactional
    public void createAttribute(
            AttributeCreateRequest attributeCreateRequest
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        AttributeTemplate attributeTemplate = mapper
                .AttributeCreateRequestToAttributeTemplate(attributeCreateRequest);
        AttributeAmount attributeAmount = mapper
                .AttributeCreateRequestToAttributeAmount(attributeCreateRequest);
        User owner = attributeTemplate.getOwner();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        if(attributeRepository.existsById(attributeTemplate.getId())){
            attributeAmountRepository.save(attributeAmount);
        } else {
            attributeTemplate = attributeRepository.saveAndFlush(attributeTemplate);
            attributeAmount.setAttributeId(attributeTemplate.getId());
            attributeAmountRepository.save(attributeAmount);
        }
    }


    @Transactional
    public void updateTemplate (
            AttributeTemplateDto attributeTemplateDto
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        AttributeTemplate attributeTemplate = mapper
                .AttributeTemplateDtoToAttributeTemplate(attributeTemplateDto);
        Long attributeId = attributeTemplate.getId();
        if (!attributeRepository.existsById(attributeId)) {
            throw new AttributeNotFoundException("Аттрибут не найден");
        }
        if (userRepository.existById(attributeTemplate.getOwner().getId())){
            throw new UserNotFoundException("Пользователь не найден");
        }
        User owner = attributeTemplate.getOwner();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        AttributeTemplate attribute = attributeRepository.findById(attributeId).get();
        attribute.setOwner(attributeTemplate.getOwner());
        attribute.setName(attributeTemplate.getName());
        attribute.setDescription(attributeTemplate.getDescription());
        attribute.setSimulationFunction(attributeTemplate.getSimulationFunction());
        attribute.setSimulationType(attributeTemplate.getSimulationType());
        attribute.setIsPrivate(attributeTemplate.getIsPrivate());

        attributeRepository.save(attribute);
    }
    @Transactional
    public void updateAttribute(
            AttributeDto attributeDto
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }

        AttributeTemplate attributeTemplate = mapper
                .AttributeDtoToAttributeTemplate(attributeDto);
        Long attributeId = attributeTemplate.getId();

        if (!attributeRepository.existsById(attributeId)) {
            throw new AttributeNotFoundException("Аттрибут не найден");
        }
        Long userId = attributeTemplate.getOwner().getId();
        if (!userRepository.existById(userId)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        AttributeAmount attributeAmount = mapper
                .AttributeDtoToAttributeAmount(attributeDto);
        Long entityId = attributeAmount.getEntityId();
        if (!deviceRepository.existById(entityId)){
            throw new EntityNotFoundException("Сущность не найдена");
        }
        AttributeTemplateDto attributeTemplateDto = mapper
                .AttributeTemplateToAttributeTemplateDto(attributeTemplate);
        updateTemplate(attributeTemplateDto);

        if (!attributeAmountRepository.existByEntityIdAndAttributeId(entityId,attributeId)){
            throw new RelationEntityException("Данный атрибут не принадлижит этой сущности");
        }
        User owner = deviceRepository.findById(entityId).get().getUser();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        if (attributeDto.getCreateNew() && !Objects.equals(attributeTemplate, attributeRepository.findById(attributeId).get()))
        {
            attributeTemplate = attributeRepository.saveAndFlush(attributeTemplate);
            deleteRelationById(entityId,attributeId);
            attributeAmountRepository.save(
                    AttributeAmount
                            .builder()
                            .attributeId(attributeTemplate.getId())
                            .entityId(entityId)
                            .startingValue(attributeAmount.getStartingValue())
                            .build());
        } else {
            AttributeAmount amount = attributeAmountRepository.findByEntityIdAndAttributeId(entityId, attributeId).get();
            amount.setStartingValue(attributeAmount.getStartingValue());
            attributeAmountRepository.save(amount);
        }
    }

    @Transactional
    public void deleteTemplateById(
            Long id
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        if (!attributeRepository.existsById(id)){
            throw new AttributeNotFoundException("Аттрибут не найден");
        }
        User owner = attributeRepository.findById(id).get().getOwner();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        attributeRepository.deleteById(id);
        if (attributeAmountRepository.existByAttributeId(id)) {
            attributeAmountRepository.deleteByAttributeId(id);
        }

    }

    @Transactional
    public void deleteRelationById(
            Long entityId,
            Long attributeId
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        if (!attributeAmountRepository.existByEntityIdAndAttributeId(entityId,attributeId)){
            throw new RelationEntityException("Данный атрибут не принадлижит этой сущности");
        }
        User owner = deviceRepository.findById(entityId).get().getUser();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        attributeAmountRepository.deleteByEntityIdAndAttributeId(entityId,attributeId);
    }


    public AttributeTemplateInfoResponse findAttributeTemplateById(
            Long id
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        if (!attributeRepository.existsById(id)) {
            throw new AttributeNotFoundException("Аттрибут не найден");
        }
        AttributeTemplate attributeTemplate = attributeRepository.findById(id).get();
        User owner = attributeTemplate.getOwner();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        AttributeTemplateInfoResponse response = mapper
                .AttributeTemplateToAttributeTemplateInfoResponse(attributeTemplate);
        return  response;
    }

    public AttributeInfoResponse findAttributeById(
            Long attributeId,
            Long entityId
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        if (!attributeRepository.existsById(attributeId)) {
            throw new AttributeNotFoundException("Аттрибут не найден");
        }
        if (!attributeAmountRepository.existByEntityIdAndAttributeId(entityId,attributeId)){
            throw new RelationEntityException("Данный атрибут не принадлижит этой сущности");
        }
        User owner = deviceRepository.findById(entityId).get().getUser();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        AttributeTemplate attributeTemplate = attributeRepository.findById(attributeId).get();
        AttributeAmount attributeAmount = attributeAmountRepository
                .findByEntityIdAndAttributeId(entityId,attributeId).get();

        AttributeInfoResponse response = mapper
                .AttributeTemplateAndAmountToAttributeInfoResponse(attributeTemplate, attributeAmount);
        return response;
    }
    public List<AttributeTemplateInfoResponse> findAllAttributesTemplateByUserId(
            Long userId
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user) || !userRepository.existById(userId)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        List<AttributeTemplate> templates = attributeRepository.findAllAttributeTemplateByOwner(userId);
        return templates.stream()
                .map(mapper::AttributeTemplateToAttributeTemplateInfoResponse)
                .collect(Collectors.toList());

    }
    public List<AttributeInfoResponse> findAllAttributesByEntityId(
            Long entityId
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException("Пользователь не найден");
        }
        if (!deviceRepository.existById(entityId)){
            throw new EntityNotFoundException("Сущность не найдена");
        }
        User owner = deviceRepository.findById(entityId).get().getUser();
        if (!owner.equals(user)){
            throw new AccessDeniedException("Доступ запрещен");
        }
        List<AttributeTemplate> templates = attributeRepository.findAllByEntityId(entityId);
        List<AttributeAmount> relations = attributeAmountRepository.findAllByEntityId(entityId);
        List<AttributeInfoResponse> responses = new ArrayList<>();
/*      По хорошему нужно переписать на Stream API
        return  templates.stream()
                .map(mapper.AttributeTemplateAndAmountToAttributeInfoResponse(());*/
        for(AttributeTemplate template : templates){
            for (AttributeAmount relation : relations){
                if(template.getId().equals(relation.getAttributeId())){
                    responses.add(mapper
                            .AttributeTemplateAndAmountToAttributeInfoResponse(template, relation));
                }
            }
        }
        return  responses;
    }
}
