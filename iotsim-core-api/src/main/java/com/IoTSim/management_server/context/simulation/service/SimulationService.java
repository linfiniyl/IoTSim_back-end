package com.IoTSim.management_server.context.simulation.service;

import com.IoTSim.management_server.api.exceptions.DeviceNotFoundException;
import com.IoTSim.management_server.api.exceptions.SimulationNotFoundException;
import com.IoTSim.management_server.api.exceptions.UserNotFoundException;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.device.repository.DevicesAmountRepository;
import com.IoTSim.management_server.context.simulation.api.SimulationCreateRequest;
import com.IoTSim.management_server.context.simulation.api.SimulationInfoResponse;
import com.IoTSim.management_server.context.simulation.dto.SimulationDto;
import com.IoTSim.management_server.context.simulation.dto.SimulationInstanceDto;
import com.IoTSim.management_server.context.simulation.dto.SimulationProcessDto;
import com.IoTSim.management_server.context.simulation.mapper.SimulationMapper;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.simulation.model.SimulationInstance;
import com.IoTSim.management_server.context.simulation.model.SimulationProcess;
import com.IoTSim.management_server.context.simulation.repository.SimulationInstanceRepository;
import com.IoTSim.management_server.context.simulation.repository.SimulationProcessRepository;
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

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

@Slf4j
@Service
@RequiredArgsConstructor
public class SimulationService {

    private final SimulationRepository simulationRepository;
    private final UserRepository userRepository;
    private final SimulationMapper mapper;
    private final DevicesAmountRepository devicesAmountRepository;
    private final SimulationProcessRepository simulationProcessRepository;
    private final SimulationInstanceRepository simulationInstanceRepository;
    private  final DeviceRepository deviceRepository;


    @Transactional
    public SimulationDto createSimulation(SimulationCreateRequest request) {
        log.debug("Creating simulation with request: {}", request);

        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found during simulation creation");
            throw new UserNotFoundException();
        }

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new );

        Simulation simulation = Simulation.builder()
                .name(request.getName())
                .isPrivate(request.getIsPrivate())
                .user(user)
                .build();

        SimulationDto simulationDto = mapper.SimulationToSimulationDto(simulationRepository.saveAndFlush(simulation));
        log.info("Simulation created with ID: {}", simulationDto.getId());
        return simulationDto;
    }

    @Transactional
    public void updateSimulation(
            SimulationDto simulationDto
    ) {
        log.info("Updating simulation with ID: {}", simulationDto.getId());

        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found during simulation update");
            throw new UserNotFoundException();
        }

        Long owner = simulationDto.getUserId();
        if (!userRepository.existById(owner)) {
            log.error("User with ID: {} does not exist", owner);
            throw new UserNotFoundException();
        }

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!Objects.equals(user.getId(), owner)) {
            log.error("Access denied for user ID: {} to update simulation ID: {}", user.getId(), simulationDto.getId());
            throw new AccessDeniedException("Access denied");
        }

        if (!simulationRepository.existsById(simulationDto.getId())) {
            log.error("Simulation not found with ID: {}", simulationDto.getId());
            throw new SimulationNotFoundException();
        }

        Simulation simulation = mapper.SimulationDtoToSimulation(simulationDto);
        simulationRepository.save(simulation);
        log.info("Simulation updated with ID: {}", simulationDto.getId());
    }

    @Transactional
    public void deleteSimulationById(Long id) {
        log.debug("Deleting simulation with ID: {}", id);

        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found during simulation deletion");
            throw new UserNotFoundException();
        }

        User owner = simulationRepository.findById(id)
                .orElseThrow(() -> {
                    log.error("Simulation not found with ID: {}", id);
                    return new SimulationNotFoundException();
                })
                .getUser();

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);


        if (!Objects.equals(user.getId(), owner.getId())) {
            log.error("Access denied for user ID: {} to delete simulation ID: {}", user.getId(), id);
            throw new AccessDeniedException("Access denied");
        }

        simulationRepository.deleteById(id);
        log.info("Simulation deleted with ID: {}", id);
    }

    @Transactional
    public SimulationInfoResponse findById(Long id) {
        log.debug("Finding simulation by ID: {}", id);

        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found during simulation retrieval");
            throw new UserNotFoundException();
        }

        Simulation simulation = simulationRepository.findById(id).orElseThrow(() -> {
            log.error("Simulation not found with ID: {}", id);
            return new SimulationNotFoundException();
        });
        User owner = simulation.getUser();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);


        if (!Objects.equals(user.getId(), owner.getId()) && simulation.getIsPrivate()) {
            log.error("Access denied for user ID: {} to view private simulation ID: {}", user.getId(), id);
            throw new AccessDeniedException("Access denied");
        }

        SimulationInfoResponse response = mapper
                .SimulationToSimulationInfoResponse(simulation);
        response.setAmountDevice(
                devicesAmountRepository
                    .sumDevicesBySimulationId(
                            response.getId()
                    )
        );
        Set<SimulationProcess> simulationProcessSet = simulationProcessRepository
                .findBySimulationId(
                        simulation.getId()
                );

        Set<SimulationProcessDto> simulationProcessDtoSet = new HashSet<>();
        Queue<Set<SimulationInstanceDto>> simulationInstanceDtoQueue = new LinkedList<>();
        for (SimulationProcess process : simulationProcessSet){
            Set<SimulationInstance> simulationInstanceSet = simulationInstanceRepository
                    .findBySimulationProcessId(process.getId());

            Device device = deviceRepository.findById(process.getDeviceId()).orElseThrow(DeviceNotFoundException::new);
            simulationInstanceDtoQueue.add(new HashSet<>(){{
                for(SimulationInstance instance : simulationInstanceSet){
                    add(SimulationInstanceDto
                            .builder()
                            .deviceNumber(instance.getDeviceNumber())
                            .id(instance.getId())
                            .status(instance.getStatus())
                            .processTime(Duration.between(
                                            instance.getProcessStartingTime(),
                                            LocalDateTime.now()
                                        ).getSeconds()
                            )
                            .build()
                    );
                }
            }});

            simulationProcessDtoSet.add(
                    SimulationProcessDto
                            .builder()
                            .id(process.getId())
                            .deviceName(device.getName())
                            .userId(user.getId())
                            .simulationInstanceDtoSet(simulationInstanceDtoQueue.poll())
                            .build()
            );
        }
        response.setSimulationProcessDtoSet(simulationProcessDtoSet);
        log.info("Successfully retrieved simulation info for ID: {}", id);
        return response;
    }

    @Transactional
    public List<SimulationInfoResponse> findAllAvailableSimulations() {
        log.debug("Finding all available simulations");

        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found during retrieval of available simulations");
            throw new UserNotFoundException();
        }

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        List<SimulationInfoResponse> simulations = mapper.SimulationListToSimulationInfoResponseList(
                simulationRepository.findAllByIsPrivateFalseAndUserId(user.getId())
        );

        simulations.forEach(response -> response.setAmountDevice(
                devicesAmountRepository.sumDevicesBySimulationId(response.getId())
        ));
        log.info("Successfully retrieved available simulations for user ID: {}", user.getId());
        return simulations;
    }









}
