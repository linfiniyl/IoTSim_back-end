package executions_controller.com.code_generation_module.service;


import executions_controller.com.code_generation_module.entities.DevicesAmount;
import executions_controller.com.code_generation_module.entities.Simulation;
import executions_controller.com.code_generation_module.exceptions.DeviceNotFoundException;
import executions_controller.com.code_generation_module.exceptions.RelationDeviceException;
import executions_controller.com.code_generation_module.exceptions.SimulationNotFoundException;
import executions_controller.com.code_generation_module.repository.DeviceRepository;
import executions_controller.com.code_generation_module.repository.DevicesAmountRepository;
import executions_controller.com.code_generation_module.repository.SimulationRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class DevicesAmountService {

    private final DevicesAmountRepository devicesAmountRepository;
    private final SimulationRepository simulationRepository;
    private final DeviceRepository deviceRepository;

    @Transactional
    public List<DevicesAmount> findAllRelationsBySimulationId(
            Long simulationId
    ){

        if (!simulationRepository.existsById(simulationId)){
            throw new SimulationNotFoundException();
        }

        return devicesAmountRepository.findAllDevicesById(simulationId);
    }

    @Transactional
    public DevicesAmount findById(
            Long simulationId,
            Long deviceId
    ){

        if (!devicesAmountRepository.existsBySimulationIdAndDeviceId(simulationId, deviceId)){
            throw new RelationDeviceException();
        }
        if (!simulationRepository.existsById(simulationId)){
            throw new SimulationNotFoundException();
        }
        if (!deviceRepository.existById(deviceId)){
            throw new DeviceNotFoundException();
        }

        return devicesAmountRepository.findAmountById(simulationId, deviceId).get();
    }

}
