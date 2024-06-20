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
import com.IoTSim.management_server.context.simulation.model.Status;
import com.IoTSim.management_server.context.simulation.repository.SimulationInstanceRepository;
import com.IoTSim.management_server.context.simulation.repository.SimulationProcessRepository;
import com.IoTSim.management_server.context.simulation.repository.SimulationRepository;

import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.mapper.UserMapper;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;


@Service
@RequiredArgsConstructor
public class SimulationService {

    private final SimulationRepository simulationRepository;
    private final UserRepository userRepository;
    private final SimulationMapper mapper;
    private final UserMapper userMapper;
    private final DevicesAmountRepository devicesAmountRepository;
    private final SimulationProcessRepository simulationProcessRepository;
    private final SimulationInstanceRepository simulationInstanceRepository;
    private  final DeviceRepository deviceRepository;

    @Transactional
    public SimulationDto createSimulation(
            SimulationCreateRequest request
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        Simulation simulation = Simulation
                .builder()
                .name(request.getName())
                .isPrivate(request.getIsPrivate())
                .simulationStatus(Status.NOT_RUNNING)
                .user(user)
                .build();
        return mapper.SimulationToSimulationDto(
                simulationRepository.saveAndFlush(simulation)
        );
    }

    @Transactional
    public void updateSimulation(
            SimulationDto simulationDto
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Long owner = simulationDto.getUserId();
        if (!userRepository.existById(owner)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(user.getId(), owner)){
            throw new AccessDeniedException("Access denied");
        }
        if(!simulationRepository.existsById(simulationDto.getId())){
            throw new SimulationNotFoundException();
        }
        Simulation simulation = mapper.SimulationDtoToSimulation(simulationDto);
        simulationRepository.save(simulation);
    }

    @Transactional
    public void deleteSimulationById(Long id) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        User owner = simulationRepository.findById(id).orElseThrow(SimulationNotFoundException::new).getUser();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!Objects.equals(user.getId(), owner.getId())){
            throw new AccessDeniedException("Access denied");
        }
        simulationRepository.deleteById(id);
    }

    @Transactional
    public SimulationInfoResponse findById(Long id) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        Simulation simulation = simulationRepository.findById(id).orElseThrow(SimulationNotFoundException::new);
        User owner = simulation.getUser();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!Objects.equals(user.getId(), owner.getId()) && simulation.getIsPrivate()){
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

        return response;
    }

    @Transactional
    public List<SimulationInfoResponse> findAllAvailableSimulations() {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        List<SimulationInfoResponse> simulations = mapper.SimulationListToSimulationInfoResponseList(
                simulationRepository.findAllByIsPrivateFalseAndUserId(user.getId())
        );
        simulations.forEach(response -> response.setAmountDevice(
                devicesAmountRepository.sumDevicesBySimulationId(response.getId())
        ));
        return simulations;
    }

    public List<Simulation> findAll() {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        return simulationRepository.findAll();
    }

    @Transactional
    public long count(
            UserDto userDto
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user) ||
                !userRepository.existById(userDto.getId())){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!Objects.equals(user.getId(), userDto.getId())){
            throw new AccessDeniedException("Access denied");
        }

        return simulationRepository
                .countByIsPrivateFalseAndUser(
                        userMapper.UserDtoToUser(userDto)
                );
    }


}
