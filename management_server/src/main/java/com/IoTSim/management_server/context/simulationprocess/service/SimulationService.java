package com.IoTSim.management_server.context.simulationprocess.service;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.simulationprocess.repository.SimulationRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class SimulationService {

    @Autowired
    private DeviceRepository deviceRepository;
    @Autowired
    private SimulationRepository simulationRepository;

    public Simulation createSimulation(Simulation simulation) {
        return simulationRepository.save(simulation);
    }

    public Simulation update(Simulation simulation, Long userId, Long id) {
        Optional<Simulation> optionalSimulation = simulationRepository.findById(userId, id);
        if (optionalSimulation.isPresent()){
            Simulation existingSimulation = optionalSimulation.get();
            existingSimulation.setName(simulation.getName());
            existingSimulation.setRoutes(simulation.getRoutes());
            existingSimulation.setStatus(simulation.getStatus());
            return simulationRepository.save(existingSimulation);
        } else {
            return null;
        }
    }

    public void deleteById(Long id, Long userId) {
       simulationRepository.deleteById(userId, id);
    }


    public Optional<Simulation> findById(Long userId, Long id) {
        return simulationRepository.findById(userId, id);
    }


    public List<Simulation> findAllByUserId(Long id) {
        return simulationRepository.findAllSimulationByUserId(id);
    }


    public List<Simulation> findAll() {
        return simulationRepository.findAll();
    }

    public long count() {
        return simulationRepository.count();
    }


}
