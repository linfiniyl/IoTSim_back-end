package com.IoTSim.management_server.context.device.service;

import com.IoTSim.management_server.context.device.model.DevicesAmount;
import com.IoTSim.management_server.context.device.repository.DevicesAmountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class DevicesAmountService {

    @Autowired
    private DevicesAmountRepository devicesAmountRepository;

    public List<DevicesAmount> findAllById(Long userId, Long id){
        return devicesAmountRepository.findAllEntitiesById(userId, id);
    }
    public Optional<DevicesAmount> findById(Long userId, Long id, Long entityId){
        return devicesAmountRepository.findEntityById(userId, id, entityId);
    }

    public void createEntitiesAmount(DevicesAmount devicesAmount){
        devicesAmountRepository.save(devicesAmount);
    }

    public void deleteById(Long userId, Long id, Long entityId){
        devicesAmountRepository.deleteEntitiesAmounById(userId, id, entityId);
    }

    public DevicesAmount update(DevicesAmount devicesAmount, Long userId, Long id, Long entityId){
        Optional<DevicesAmount> optionalEntity = devicesAmountRepository.findEntityById(userId, id, entityId);
        if (optionalEntity.isPresent()){
            DevicesAmount existingEntity = optionalEntity.get();
            existingEntity.setAmount(devicesAmount.getAmount());
            return devicesAmountRepository.save(existingEntity);
        } else {
            return null;
        }
    }
}
