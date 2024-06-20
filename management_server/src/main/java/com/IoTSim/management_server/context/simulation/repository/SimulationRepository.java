package com.IoTSim.management_server.context.simulation.repository;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Optional;


public interface SimulationRepository extends JpaRepository<Simulation, Long> {


    @NonNull
    Optional<Simulation> findById(@NonNull Long id);

    @Override
    boolean existsById(@NonNull Long id);

    void deleteById(@NonNull Long id);

    long countByIsPrivateFalseAndUser(User user);

    @Query("select s from Simulation s where s.isPrivate = false and s.user.id = ?1")
    List<Simulation> findAllByIsPrivateFalseAndUserId(long id);


    @Query("select count(s) from Simulation s inner join s.amount amount where amount.simulationId = ?1")
    long countByAmountSimulationId(Long simulationId);



}
