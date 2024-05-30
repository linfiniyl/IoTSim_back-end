package com.IoTSim.management_server.context.simulationprocess.repository;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;


public interface SimulationRepository extends JpaRepository<Simulation, Long> {

    @Query(value = "SELECT * FROM simulation WHERE user_id = ?1" , nativeQuery = true)
    List<Simulation> findAllSimulationByUserId(Long userId);
    @Query(value = "SELECT * FROM simulation WHERE user_id = ?1 AND id = ?2" , nativeQuery = true)
    Optional<Simulation> findById(Long userId, Long id);
    @Query(value = "DELETE FROM simulation WHERE user_id = ?1 AND id = ?2" , nativeQuery = true)
    void deleteById(Long userId, Long id);

}
