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
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Objects;

@Slf4j
@Service
@RequiredArgsConstructor
public class DevicesAmountService {

    private final DevicesAmountRepository devicesAmountRepository;
    private final SimulationRepository simulationRepository;
    private final DeviceRepository deviceRepository;
    private final DeviceMapper mapper;

    private final UserRepository userRepository;

    @Transactional
    public List<DevicesAmountInfoResponse> findAllRelationsBySimulationId(Long simulationId) {
        log.debug("Finding all relations by simulation ID: {}", simulationId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found");
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Simulation simulation = simulationRepository.findById(simulationId).orElseThrow(() -> {
            log.error("Simulation not found with ID: {}", simulationId);
            return new SimulationNotFoundException();
        });

        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()) {
            log.error("Access denied for user: {} on private simulation ID: {}", user.getId(), simulationId);
            throw new AccessDeniedException("Access Denied");
        }

        List<DevicesAmountInfoResponse> response = mapper.devicesAmountListToDevicesAmountInfoResponseList(
                devicesAmountRepository.findAllDevicesById(simulationId)
        );
        log.info("Successfully retrieved all relations for simulation ID: {}", simulationId);
        return response;
    }

    @Transactional
    public DevicesAmountInfoResponse findById(
            Long simulationId,
            Long deviceId
    ) {
        log.debug("Finding device amount by simulation ID: {} and device ID: {}", simulationId, deviceId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found");
            throw new UserNotFoundException();
        }

        Simulation simulation = simulationRepository.findById(simulationId).orElseThrow(() -> {
            log.error("Simulation not found with ID: {}", simulationId);
            return new SimulationNotFoundException();
        });

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository.findById(deviceId).orElseThrow(() -> {
            log.error("Device not found with ID: {}", deviceId);
            return new DeviceNotFoundException();
        });

        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()) {
            log.error("Access denied for user: {} on simulation ID: {} and device ID: {}", user.getId(), simulationId, deviceId);
            throw new AccessDeniedException("Access Denied");
        }

        DevicesAmountInfoResponse response = mapper.devicesAmountToDevicesAmountInfoResponse(
                devicesAmountRepository.findAmountById(simulationId, deviceId)
                        .orElseThrow(() -> {
                            log.error("Relation device not found for simulation ID: {} and device ID: {}", simulationId, deviceId);
                            return new RelationDeviceException();
                        })
        );

        log.info("Successfully retrieved device amount for simulation ID: {} and device ID: {}", simulationId, deviceId);
        return response;
    }

    @Transactional
    public DevicesAmountDto createDevicesAmount(
            DevicesAmountCreateRequest request
    ) {
        log.debug("Creating devices amount with request: {}", request);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found");
            throw new UserNotFoundException();
        }

        Simulation simulation = simulationRepository.findById(request.getSimulationId()).orElseThrow(() -> {
            log.error("Simulation not found with ID: {}", request.getSimulationId());
            return new SimulationNotFoundException();
        });
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);


        Device device = deviceRepository.findById(request.getDeviceId()).orElseThrow(() -> {
            log.error("Device not found with ID: {}", request.getDeviceId());
            return new DeviceNotFoundException();
        });

        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()) {
            log.error("Access denied for user: {} while creating device amount for simulation ID: {} and device ID: {}", user.getId(), simulation.getId(), device.getId());
            throw new AccessDeniedException("Access Denied");
        }

        DevicesAmount amount = DevicesAmount.builder()
                .simulationId(request.getSimulationId())
                .deviceId(request.getDeviceId())
                .amount(request.getAmount())
                .build();

        DevicesAmountDto response = mapper.devicesAmountToDevicesAmountDto(
                devicesAmountRepository.saveAndFlush(amount)
        );

        log.info("Successfully created devices amount for simulation ID: {} and device ID: {}", request.getSimulationId(), request.getDeviceId());
        return response;
    }

    @Transactional
    public void deleteRelationsById(
            Long simulationId,
            Long deviceId
    ) {
        log.info("Deleting relations by simulation ID: {} and device ID: {}", simulationId, deviceId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();


        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found");
            throw new UserNotFoundException();
        }

        if (!devicesAmountRepository.existsBySimulationIdAndDeviceId(simulationId, deviceId)) {
            throw new RelationDeviceException();
        }
        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository.findById(deviceId).orElseThrow(() -> {
            log.error("Device not found with ID: {}", deviceId);
            return new DeviceNotFoundException();
        });

        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()) {
            log.error("Access denied for user: {} while deleting relation for simulation ID: {} and device ID: {}", user.getId(), simulationId, deviceId);
            throw new AccessDeniedException("Access Denied");
        }

        devicesAmountRepository.deleteDevicesAmountById(simulationId, deviceId);
        log.info("Successfully deleted relation for simulation ID: {} and device ID: {}", simulationId, deviceId);
    }

    @Transactional
    public void updateRelation(
            DevicesAmountDto devicesAmountDto
    ) {
        log.debug("Updating relation with DevicesAmountDto: {}", devicesAmountDto);

        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found during update relation");
            throw new UserNotFoundException();
        }

        if (!devicesAmountRepository.existsBySimulationIdAndDeviceId(
                devicesAmountDto.getSimulationId(),
                devicesAmountDto.getDeviceId())) {
            log.error("Relation not found for simulation ID: {} and device ID: {}",
                    devicesAmountDto.getSimulationId(), devicesAmountDto.getDeviceId());
            throw new RelationDeviceException();
        }

        Simulation simulation = simulationRepository
                .findById(devicesAmountDto.getSimulationId())
                .orElseThrow(() -> {
                    log.error("Simulation not found with ID: {}", devicesAmountDto.getSimulationId());
                    return new SimulationNotFoundException();
                });

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        Device device = deviceRepository
                .findById(devicesAmountDto.getDeviceId())
                .orElseThrow(() -> {
                    log.error("Device not found with ID: {}", devicesAmountDto.getDeviceId());
                    return new DeviceNotFoundException();
                });

        if (!Objects.equals(user.getId(), simulation.getUser().getId()) && simulation.getIsPrivate()
                || !Objects.equals(user.getId(), device.getUser().getId()) && device.getIsPrivate()) {
            log.error("Access denied for user ID: {} on simulation ID: {} and device ID: {}",
                    user.getId(), devicesAmountDto.getSimulationId(), devicesAmountDto.getDeviceId());
            throw new AccessDeniedException("Access Denied");
        }

        devicesAmountRepository.save(mapper.deviceAmountDtoToDeviceAmount(devicesAmountDto));
        log.info("Successfully updated relation for simulation ID: {} and device ID: {}",
                devicesAmountDto.getSimulationId(), devicesAmountDto.getDeviceId());
    }
}
