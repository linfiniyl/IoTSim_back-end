package executions_controller.com.code_generation_module.Service;


import executions_controller.com.code_generation_module.Entities.Simulation;
import executions_controller.com.code_generation_module.Repository.EntityRepository;
import executions_controller.com.code_generation_module.Repository.SimulationRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class SimulationService {

    @Autowired
    private EntityRepository entityRepository;
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
