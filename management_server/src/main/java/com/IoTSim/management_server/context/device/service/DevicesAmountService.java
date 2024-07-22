package com.IoTSim.management_server.context.device.service;

import com.IoTSim.management_server.api.exceptions.DeviceNotFoundException;
import com.IoTSim.management_server.api.exceptions.RelationDeviceException;
import com.IoTSim.management_server.api.exceptions.SimulationNotFoundException;
import com.IoTSim.management_server.api.exceptions.UserNotFoundException;
import com.IoTSim.management_server.context.device.api.DevicesAmountCreateRequest;
import com.IoTSim.management_server.context.device.api.DevicesAmountInfoResponse;
import com.IoTSim.management_server.context.device.dto.DevicesAmountDto;
import com.IoTSim.management_server.context.device.mapper.DeviceMapper;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.model.DevicesAmount;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.device.repository.DevicesAmountRepository;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.simulation.repository.SimulationRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
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
    private final DeviceMapper mapper;

    private final UserRepository userRepository;

    @Transactional
    public List<DevicesAmountInfoResponse> findAllRelationsBySimulationId(
            Long simulationId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        Simulation simulation = simulationRepository.findById(simulationId).orElseThrow(SimulationNotFoundException::new);
        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }
        return mapper.devicesAmountListToDevicesAmountInfoResponseList(
                devicesAmountRepository.findAllDevicesById(simulationId)
            );
    }

    @Transactional
    public DevicesAmountInfoResponse findById(
            Long simulationId,
            Long deviceId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        Simulation simulation = simulationRepository.findById(simulationId).orElseThrow(SimulationNotFoundException::new);
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository.findById(deviceId).orElseThrow(DeviceNotFoundException::new);
        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()
        ){
            throw new AccessDeniedException("Access Denied");
        }
        return mapper.devicesAmountToDevicesAmountInfoResponse(
                devicesAmountRepository.findAmountById(simulationId, deviceId)
                        .orElseThrow(RelationDeviceException::new)
        );
    }

    @Transactional
    public DevicesAmountDto createDevicesAmount(
            DevicesAmountCreateRequest request
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Simulation simulation = simulationRepository
                .findById(request.getSimulationId())
                .orElseThrow(SimulationNotFoundException::new);
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository
                .findById(request.getDeviceId())
                .orElseThrow(DeviceNotFoundException::new);
        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()
        ){
            throw new AccessDeniedException("Access Denied");
        }
        DevicesAmount amount = DevicesAmount
                .builder()
                .simulationId(request.getSimulationId())
                .deviceId(request.getDeviceId())
                .amount(request.getAmount())
                .build();
        return mapper.devicesAmountToDevicesAmountDto(
                devicesAmountRepository.saveAndFlush(amount)
        );
    }

    @Transactional
    public void deleteRelationsById(
            Long simulationId,
            Long deviceId
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        if (!devicesAmountRepository.existsBySimulationIdAndDeviceId(simulationId, deviceId)){
            throw new RelationDeviceException();
        }
        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository
                .findById(deviceId)
                .orElseThrow(DeviceNotFoundException::new);
        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()
        ){
            throw new AccessDeniedException("Access Denied");
        }
        devicesAmountRepository.deleteDevicesAmountById(simulationId, deviceId);
    }
    @Transactional
    public void updateRelation(
            DevicesAmountDto devicesAmountDto
    ){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        if (!devicesAmountRepository.existsBySimulationIdAndDeviceId(
                devicesAmountDto.getSimulationId(),
                devicesAmountDto.getDeviceId())
        ){
            throw new RelationDeviceException();
        }
        Simulation simulation = simulationRepository
                .findById(devicesAmountDto.getSimulationId())
                .orElseThrow(SimulationNotFoundException::new);
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository
                .findById(devicesAmountDto.getDeviceId())
                .orElseThrow(DeviceNotFoundException::new);
        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()
        ){
            throw new AccessDeniedException("Access Denied");
        }

        devicesAmountRepository.save(
                mapper.deviceAmountDtoToDeviceAmount(devicesAmountDto)
        );
    }
}
