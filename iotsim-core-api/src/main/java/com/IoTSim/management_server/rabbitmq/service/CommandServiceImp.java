package com.IoTSim.management_server.rabbitmq.service;

import com.IoTSim.management_server.api.exceptions.*;
import com.IoTSim.management_server.context.attribute.repository.AttributeRelationRepository;
import com.IoTSim.management_server.context.device.model.DevicesAmount;
import com.IoTSim.management_server.context.device.repository.DevicesAmountRepository;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.simulation.model.SimulationInstance;
import com.IoTSim.management_server.context.simulation.model.SimulationProcess;
import com.IoTSim.management_server.context.simulation.model.Status;
import com.IoTSim.management_server.context.simulation.repository.SimulationInstanceRepository;
import com.IoTSim.management_server.context.simulation.repository.SimulationProcessRepository;
import com.IoTSim.management_server.context.simulation.repository.SimulationRepository;
import com.IoTSim.management_server.context.user.model.User;
import lombok.RequiredArgsConstructor;
import org.json.JSONObject;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.time.LocalDateTime;
import java.util.*;

@RequiredArgsConstructor
@Service
public class CommandServiceImp implements CommandService {
    @Value("${spring.rabbitmq.routing_key}")
    private String routingKey;
    private final SimulationRepository simulationRepository;
    private final SimulationProcessRepository simulationProcessRepository;
    private final SimulationInstanceRepository simulationInstanceRepository;
    private final DevicesAmountRepository devicesAmountRepository;
    private final AttributeRelationRepository attributeRelationRepository;
    private final RabbitTemplate rabbitTemplate;

    @Override
    @Transactional
    public void startSimulation(Long simulationId) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        User owner = simulation.getUser();
        if (!Objects.equals(owner.getId(), user.getId()) && simulation.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }

        if (devicesAmountRepository.sumDevicesBySimulationId(simulationId) == 0){
            throw new SimulationWithoutDevicesException();
        }
        if (devicesAmountRepository.sumDevicesWithoutAttributesBySimulationId(simulationId) > 0){
            throw new DeviceWithoutAttributesException();
        }
        if (attributeRelationRepository.countNotDefinedAttributesBySimulationId(simulationId) > 0){
            throw new DeviceWithUndefinedAttributeException();
        }

        if (simulation.getSimulationStatus().equals(Status.IN_PROCESS)
            || simulation.getSimulationStatus().equals(Status.STARTING)
        ){
            throw new SimulationHasAlreadyStartedException();
        }
        JSONObject command = new JSONObject();
        command.put("command", "Start simulation");
        command.put("simulationId", simulationId);
        command.put("userId", user.getId());
        Message message = new Message(command.toString().getBytes());
        message.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);
        rabbitTemplate.convertAndSend(routingKey, message);
        simulation.setSimulationStatus(Status.STARTING);
        createSimulationProcesses(simulation, user);
        simulationRepository.save(simulation);
    }

    @Override
    @Transactional
    public void stopSimulation(Long simulationId) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        User owner = simulation.getUser();
        if (!Objects.equals(owner.getId(), user.getId()) && simulation.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }
        if (simulation.getSimulationStatus().equals(Status.STOPPED)){
            throw new SimulationHasAlreadyStoppedException();
        }

        JSONObject command = new JSONObject();
        command.put("command", "Stop simulation");
        command.put("simulationId", simulationId);
        command.put("userId", user.getId());
        Message message = new Message(command.toString().getBytes());
        message.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

        rabbitTemplate.convertAndSend(routingKey, message);
        simulation.setSimulationStatus(Status.STOPPING);
        simulationRepository.save(simulation);
    }
/*
    @Override
    @Transactional
    public void restartSimulation(Long simulationId) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        User owner = simulation.getUser();
        if (!Objects.equals(owner.getId(), user.getId()) && simulation.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }

        JSONObject command = new JSONObject();
        command.put("command", "Restart simulation");
        command.put("simulationId", simulationId);
        command.put("userId", user.getId());
        Message message = new Message(command.toString().getBytes());
        message.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

        rabbitTemplate.convertAndSend(routingKey, message);
        simulation.setSimulationStatus(Status.RESTARTING);
        simulationRepository.save(simulation);
    }
*/
    @Transactional
    @Override
    public void createSimulationProcesses(Simulation simulation, User user){
        Set<SimulationProcess> processSet = new LinkedHashSet<>();
        Queue<Set<SimulationInstance>> instanceSetQueue = new LinkedList<>();
        List<Long> deviceList = devicesAmountRepository
                .findAllDevicesById(simulation.getId())
                .stream()
                .map(DevicesAmount::getDeviceId)
                .toList();
        for (Long deviceId : deviceList) {
            SimulationProcess process = SimulationProcess
                    .builder()
                    .deviceId(deviceId)
                    .simulationId(simulation.getId())
                    .user(user)
                    .build();
            Long amount = devicesAmountRepository
                    .findAmountById(simulation.getId(), deviceId)
                    .orElseThrow(RelationDeviceException::new)
                    .getAmount();
            process = simulationProcessRepository.saveAndFlush(process);
            for (long i = 0; i < amount; i++) {
                long finalI = i;
                SimulationProcess finalProcess = process;
                instanceSetQueue.add(new LinkedHashSet<>(){{
                    add(
                            SimulationInstance
                                    .builder()
                                    .simulationProcess(finalProcess)
                                    .deviceNumber(finalI)
                                    .status(Status.STARTING)
                                    .processStartingTime(LocalDateTime.now())
                                    .build()

                    );
                }});
            }
            process.setSimulationInstances(instanceSetQueue.peek());
            processSet.add(process);
            simulationInstanceRepository.saveAll(Objects.requireNonNull(instanceSetQueue.poll()));
        }
        simulationProcessRepository.saveAll(processSet);
    }

    @Override
    @Transactional
    public void pauseSimulation(Long simulationId) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        User owner = simulation.getUser();
        if (!Objects.equals(owner.getId(), user.getId()) && simulation.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }
        if (simulation.getSimulationStatus().equals(Status.PAUSING)
            || simulation.getSimulationStatus().equals(Status.PAUSED)
        ){
            throw new SimulationHasAlreadyStoppedException();
        }

        JSONObject command = new JSONObject();
        command.put("command", "Pause simulation");
        command.put("simulationId", simulationId);
        command.put("userId", user.getId());
        Message message = new Message(command.toString().getBytes());
        message.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

        rabbitTemplate.convertAndSend(routingKey, message);
        simulation.setSimulationStatus(Status.PAUSING);
        simulationRepository.save(simulation);
    }

    @Override
    @Transactional
    public void continueSimulation(Long simulationId) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Simulation simulation = simulationRepository
                .findById(simulationId)
                .orElseThrow(SimulationNotFoundException::new);
        User owner = simulation.getUser();
        if (!Objects.equals(owner.getId(), user.getId()) && simulation.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }
        if (simulation.getSimulationStatus().equals(Status.IN_PROCESS)
            || simulation.getSimulationStatus().equals(Status.CONTINUING)
        ){
            throw new SimulationIsAlreadyRunningException();
        }

        JSONObject command = new JSONObject();
        command.put("command", "Continue simulation");
        command.put("simulationId", simulationId);
        command.put("userId", user.getId());
        Message message = new Message(command.toString().getBytes());
        message.getMessageProperties().setContentType(MessageProperties.CONTENT_TYPE_JSON);

        rabbitTemplate.convertAndSend(routingKey, message);
        simulation.setSimulationStatus(Status.CONTINUING);
        simulationRepository.save(simulation);
    }
}
