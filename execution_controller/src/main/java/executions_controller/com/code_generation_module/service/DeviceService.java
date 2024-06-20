package executions_controller.com.code_generation_module.service;


import executions_controller.com.code_generation_module.entities.Device;
import executions_controller.com.code_generation_module.exceptions.DeviceNotFoundException;
import executions_controller.com.code_generation_module.repository.DeviceRepository;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class DeviceService {

    private final DeviceRepository deviceRepository;

    @Transactional
    public Device findById(
            Long deviceId
    ) {
        if (!deviceRepository.existById(deviceId)){
            throw new DeviceNotFoundException();
        }

       return deviceRepository.findById(deviceId).get();
    }

}
