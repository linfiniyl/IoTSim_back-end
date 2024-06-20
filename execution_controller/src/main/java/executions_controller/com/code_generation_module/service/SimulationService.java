package executions_controller.com.code_generation_module.service;


import executions_controller.com.code_generation_module.entities.Simulation;
import executions_controller.com.code_generation_module.exceptions.SimulationNotFoundException;
import executions_controller.com.code_generation_module.repository.*;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


@Service
@RequiredArgsConstructor
public class SimulationService {

    private final SimulationRepository simulationRepository;

    @Transactional
    public Simulation findById(Long id) {

        if(!simulationRepository.existsById(id)){
            throw new SimulationNotFoundException();
        }

        return simulationRepository.findById(id).get();
    }

    @Transactional
    public void setSupervisorPID(Long pid){

    }
}
