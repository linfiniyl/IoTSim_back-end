package com.IoTSim.management_server.context.device.repository;

import com.IoTSim.management_server.context.device.model.DevicesAmount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface DevicesAmountRepository extends JpaRepository<DevicesAmount,Long> {
    @Query(value = "SELECT * FROM entities_amount WHERE user_id = ?1 and simulation_id = ?2" , nativeQuery = true)
    List<DevicesAmount> findAllEntitiesById(Long userId, Long simulation_id);

    @Query(value = "DELETE FROM entities_amount WHERE user_id = ?1 and simulation_id = ?2 and entity_id = ?3", nativeQuery = true)
    void deleteEntitiesAmounById(Long userId, Long id, Long entityId);

    @Query(value = "SELECT * FROM entities_amount " +
            "WHERE user_id = ?1 and simulation_id = ?2 and entity_id = ?3" , nativeQuery = true)
    Optional<DevicesAmount> findEntityById(Long userId, Long simulation_id, Long entityId);
}
