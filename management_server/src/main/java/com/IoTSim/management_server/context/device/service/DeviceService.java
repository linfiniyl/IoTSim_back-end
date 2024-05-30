package com.IoTSim.management_server.context.device.service;


import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class DeviceService {
    @Autowired
    private DeviceRepository deviceRepository;

    public void createEntity(Device entity) {
        deviceRepository.save(entity);
    }

    public Device update(Device entity, Long entityId) {
        Optional<Device> optionalEntity = deviceRepository.findById(entityId);
        if (optionalEntity.isPresent()){
            Device existingEntity = optionalEntity.get();
            existingEntity.setName(entity.getName());
            existingEntity.setAttibutes(entity.getAttibutes());
            existingEntity.setDescription(entity.getDescription());
            existingEntity.setPicture(entity.getPicture());
            return deviceRepository.save(existingEntity);
        } else {
            return null;
        }
    }

    public void deleteById(Long id) {
        deviceRepository.deleteById(id);
    }

    public Optional<Device> findById(Long id) {
       return deviceRepository.findById(id);
    }

    public List<Device> findAll() {
        return deviceRepository.findAll();
    }

    public long count() {
        return deviceRepository.count();
    }
}
