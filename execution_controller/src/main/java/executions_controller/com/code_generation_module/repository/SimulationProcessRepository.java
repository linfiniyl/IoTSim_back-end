package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.SimulationProcess;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;
import java.util.Set;

public interface SimulationProcessRepository extends JpaRepository<SimulationProcess, Long> {
    @Query("select s from SimulationProcess s where s.simulationId = ?1")
    Set<SimulationProcess> findBySimulationId(Long simulationId);

    @Query("select s from SimulationProcess s where s.user.id = ?1 and s.simulationId = ?2 and s.deviceId = ?3")
    Optional<SimulationProcess> findBySimulationIdAndUserIdAndDeviceId(Long id, Long simulationId, Long deviceId);

}
