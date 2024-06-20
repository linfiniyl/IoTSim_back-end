package com.IoTSim.management_server.context.attribute.service;

import com.IoTSim.management_server.api.exceptions.AttributeNotFoundException;
import com.IoTSim.management_server.api.exceptions.DeviceNotFoundException;
import com.IoTSim.management_server.api.exceptions.RelationDeviceException;
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
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;


import java.util.List;
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
    public AttributeTemplateInfoResponse createAttributeTemplate(
            AttributeTemplateCreateRequest attributeTemplateCreateRequest
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        AttributeTemplate attributeTemplate = mapper
                .AttributeTemplateCreateRequestToAttributeTemplate(attributeTemplateCreateRequest);
        User owner = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        attributeTemplate.setOwner(owner);
       return mapper
               .AttributeTemplateToAttributeTemplateInfoResponse(
                       attributeRepository.save(attributeTemplate)
               );
    }

    @Transactional
    public AttributeInfoResponse createAttribute(
            AttributeCreateRequest attributeCreateRequest
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        AttributeAmount attributeAmount = mapper
                .AttributeCreateRequestToAttributeAmount(
                        attributeCreateRequest
                );

        AttributeTemplate attribute = attributeRepository
                .findById(attributeAmount.getAttributeId())
                .orElseThrow(AttributeNotFoundException::new);
        Device device = deviceRepository
                .findById(attributeAmount.getDeviceId())
                .orElseThrow(DeviceNotFoundException::new);

        User ownerAttribute = attribute.getOwner();
        User ownerDevice = device.getUser();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!user.equals(ownerAttribute) || !user.equals(ownerDevice)){
            throw new AccessDeniedException("Access Denied");
        }
        return mapper.AttributeAmountToAttributeInfoResponse(
                attributeAmountRepository.saveAndFlush(attributeAmount)
        );
    }


    @Transactional
    public void updateTemplate (
            AttributeTemplateDto attributeTemplateDto
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        AttributeTemplate attributeTemplate = mapper
                .AttributeTemplateDtoToAttributeTemplate(attributeTemplateDto);
        Long attributeId = attributeTemplate.getId();
        if (!attributeRepository.existsById(attributeId)) {
            throw new AttributeNotFoundException();
        }
        if (!userRepository.existById(attributeTemplate.getOwner().getId())){
            throw new UserNotFoundException();
        }
        User owner = attributeTemplate.getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!owner.equals(user)){
            throw new AccessDeniedException("Access Denied");
        }

        attributeRepository.save(
                mapper.AttributeTemplateDtoToAttributeTemplate(attributeTemplateDto)
        );
    }
    @Transactional
    public void updateRelation(
            AttributeDto attributeDto
    ) throws AccessDeniedException {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        AttributeAmount attributeAmount = mapper
                .AttributeDtoToAttributeAmount(attributeDto);

        long attributeId = attributeAmount.getAttributeId();
        long deviceId = attributeAmount.getDeviceId();

        long ownerAttributeId = attributeRepository
                .findById(attributeId)
                .orElseThrow(AttributeNotFoundException::new)
                .getOwner()
                .getId();

        if (!userRepository.existById(ownerAttributeId)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (user.getId() != ownerAttributeId){
            throw new AccessDeniedException("Access Denied");
        }

        long ownerDeviceId = deviceRepository
                .findById(deviceId)
                .orElseThrow(DeviceNotFoundException::new)
                .getUser()
                .getId();
        if (!userRepository.existById(ownerDeviceId)){
            throw new UserNotFoundException();
        }
        if (user.getId() != ownerDeviceId){
            throw new AccessDeniedException("Access Denied");
        }

        if (!attributeAmountRepository.existByDeviceIdAndAttributeId(deviceId,attributeId)){
            throw new RelationDeviceException();
        }
        attributeAmountRepository.save(attributeAmount);
    }

    @Transactional
    public void deleteTemplateById(
            Long id
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        User owner = attributeRepository
                .findById(id)
                .orElseThrow(AttributeNotFoundException::new)
                .getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!owner.equals(user)){
            throw new AccessDeniedException("Access Denied");
        }
        attributeRepository.deleteById(id);
    }

    @Transactional
    public void deleteRelationById(
            Long deviceId,
            Long attributeId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        long ownerAttributeId = attributeRepository
                .findById(attributeId)
                .orElseThrow(AttributeNotFoundException::new)
                .getOwner()
                .getId();

        if (!userRepository.existById(ownerAttributeId)){
            throw new UserNotFoundException();
        }

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (user.getId() != ownerAttributeId){
            throw new AccessDeniedException("Access Denied");
        }
        if (!deviceRepository.existById(deviceId)){
            throw new DeviceNotFoundException();
        }
        long ownerDeviceId = deviceRepository
                .findById(deviceId)
                .orElseThrow(DeviceNotFoundException::new)
                .getUser()
                .getId();

        if (!userRepository.existById(ownerDeviceId)){
            throw new UserNotFoundException();
        }
        if (user.getId() != ownerDeviceId){
            throw new AccessDeniedException("Access Denied");
        }

        if (!attributeAmountRepository.existByDeviceIdAndAttributeId(deviceId,attributeId)){
            throw new RelationDeviceException();
        }
        attributeAmountRepository.deleteByDeviceIdAndAttributeId(deviceId,attributeId);
    }


    @Transactional
    public AttributeTemplateInfoResponse findAttributeTemplateById(
            Long attributeTemplateId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        AttributeTemplate attributeTemplate = attributeRepository
                .findById(attributeTemplateId)
                .orElseThrow(AttributeNotFoundException::new);
        User owner = attributeTemplate.getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!owner.equals(user)){
            throw new AccessDeniedException("Access Denied");
        }

        return mapper
                .AttributeTemplateToAttributeTemplateInfoResponse(
                        attributeTemplate
                );
    }

    @Transactional
    public AttributeInfoResponse findAttributeById(
            Long attributeId,
            Long deviceId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        long ownerAttributeId = attributeRepository
                .findById(attributeId)
                .orElseThrow(AttributeNotFoundException::new)
                .getOwner()
                .getId();

        if (!userRepository.existById(ownerAttributeId)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (user.getId() != ownerAttributeId){
            throw new AccessDeniedException("Access Denied");
        }

        long ownerDeviceId = deviceRepository
                .findById(deviceId)
                .orElseThrow(DeviceNotFoundException::new)
                .getUser()
                .getId();

        if (!userRepository.existById(ownerDeviceId)){
            throw new UserNotFoundException();
        }
        if (user.getId() != ownerDeviceId){
            throw new AccessDeniedException("Access Denied");
        }

        AttributeAmount attributeAmount = attributeAmountRepository
                .findByDeviceIdAndAttributeId(deviceId,attributeId)
                .orElseThrow(RelationDeviceException::new);

        return mapper
                .AttributeAmountToAttributeInfoResponse(
                        attributeAmount
                );
    }
    @Transactional
    public List<AttributeTemplateInfoResponse> findAllAttributesTemplateByUserId() {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        List<AttributeTemplate> templates = attributeRepository
                .findAllAttributesByOwnerIdAndIsPrivateFalse(
                        user.getId()
                );
        return templates.stream()
                .map(mapper::AttributeTemplateToAttributeTemplateInfoResponse)
                .collect(Collectors.toList());

    }
    @Transactional
    public List<AttributeInfoResponse> findAllAttributesByDeviceId(
            Long deviceId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        User owner = deviceRepository
                .findById(deviceId)
                .orElseThrow(DeviceNotFoundException::new)
                .getUser();

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!owner.equals(user)){
            throw new AccessDeniedException("Access Denied");
        }
        return mapper.AttributeAmountListToAttributeInfoResponseList(
                attributeAmountRepository.findAllByDeviceId(deviceId)
        );
    }
}
