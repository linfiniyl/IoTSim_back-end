package com.IoTSim.management_server.context.simulation.repository;

import com.IoTSim.management_server.context.simulation.model.SimulationProcess;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface SimulationProcessRepository extends JpaRepository<SimulationProcess, Long> {
    @Query("select s from SimulationProcess s where s.simulationId = ?1")
    Set<SimulationProcess> findBySimulationId(Long simulationId);

    @Query("select s from SimulationProcess s where s.simulationId = ?1 and s.userId = ?2")
    SimulationProcess findBySimulationIdAndUserId(Long simulationId, Long userId);
}
