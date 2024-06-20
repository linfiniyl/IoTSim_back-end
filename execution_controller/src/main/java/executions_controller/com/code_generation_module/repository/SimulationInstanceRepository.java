package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.SimulationInstance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface SimulationInstanceRepository extends JpaRepository<SimulationInstance, Long> {

    @Query("select s from SimulationInstance s where s.simulationProcess.id = ?1")
    Set<SimulationInstance> findBySimulationProcessId(Long id);


}
