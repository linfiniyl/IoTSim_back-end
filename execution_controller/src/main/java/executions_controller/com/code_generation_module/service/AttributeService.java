package executions_controller.com.code_generation_module.service;


import executions_controller.com.code_generation_module.entities.AttributeAmount;
import executions_controller.com.code_generation_module.entities.AttributeTemplate;
import executions_controller.com.code_generation_module.exceptions.AttributeNotFoundException;
import executions_controller.com.code_generation_module.exceptions.DeviceNotFoundException;
import executions_controller.com.code_generation_module.exceptions.RelationDeviceException;
import executions_controller.com.code_generation_module.exceptions.SimulationNotFoundException;
import executions_controller.com.code_generation_module.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


@RequiredArgsConstructor
@Service
public class AttributeService {

    private final AttributeTemplateRepository attributeRepository;
    private final AttributeAmountRepository attributeAmountRepository;
    private final SimulationRepository simulationRepository;
    private final DeviceRepository deviceRepository;


    @Transactional
    public AttributeTemplate findAttributeTemplateById(
            Long attributeTemplateId
    ){
        if (!attributeRepository.existsById(attributeTemplateId)) {
            throw new AttributeNotFoundException();
        }

        return attributeRepository.findById(attributeTemplateId).get();
    }

    @Transactional
    public AttributeAmount findAttributeById(
            Long attributeId,
            Long deviceId,
            Long simulationId,
            Long userId
    ){

        if (!attributeRepository.existsById(attributeId)) {
            throw new AttributeNotFoundException();
        }

        if (!deviceRepository.existById(deviceId)){
            throw new DeviceNotFoundException();
        }
        if (!simulationRepository.existsById(simulationId)){
            throw new SimulationNotFoundException();
        }

        if (!attributeAmountRepository.existByDeviceIdAndAttributeId(deviceId,attributeId)){
            throw new RelationDeviceException();
        }

        return attributeAmountRepository
                .findByAttributeIdAndDeviceIdAndSimulationIdAndUserId(
                        deviceId,
                        attributeId,
                        simulationId,
                        userId
                );
    }

    @Transactional
    public List<AttributeAmount> findAllAttributesByDeviceId(
            Long deviceId,
            Long simulationId,
            Long userId
    ){

        if (!deviceRepository.existById(deviceId)){
            throw new DeviceNotFoundException();
        }

        return attributeAmountRepository.findAllByDeviceId(deviceId, simulationId, userId);
    }
}
