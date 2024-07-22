package com.IoTSim.management_server.context.attribute.repository;

import com.IoTSim.management_server.context.attribute.api.AttributeInfoResponse;
import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeAmountId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

@Repository
public interface AttributeAmountRepository extends JpaRepository<AttributeAmount, AttributeAmountId> {
    @Query(value = "SELECT * FROM attribute_amount WHERE device_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByDeviceId(Long deviceId);

    @Query(value = "SELECT * FROM attribute_amount WHERE attribute_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByAttributeId(Long attributeId);

    @Query("select (count(a) > 0) from AttributeAmount a where a.deviceId = ?1 and a.attributeId = ?2 and a.userId = ?3 and a.simulationId = ?4")
    boolean existsById(Long deviceId, Long attributeId, Long userId, Long simulationId);

    @Modifying
    @Query(value = "DELETE FROM attribute_amount WHERE device_id = ?1 and attribute_id = ?2 and a.userId = ?3 and a.simulationId = ?4" , nativeQuery = true)
    void deleteById(Long deviceId, Long attributeId, Long userId, Long simulationId);

    @Query("""
            select a from AttributeAmount a
            where a.deviceId = :deviceId and a.attributeId = :attributeId and a.userId = :userId and a.simulationId = :simulationId""")
    Optional<AttributeAmount> findByDeviceIdAndAttributeIdAndUserIdAndSimulationId(@Param("deviceId") Long deviceId, @Param("attributeId") Long attributeId, @Param("userId") Long userId, @Param("simulationId") Long simulationId);


    @Query("""
            select a from AttributeAmount a
            where a.deviceId = :deviceId and a.userId = :userId and a.simulationId = :simulationId""")
    List<AttributeAmount> findByDeviceIdAndUserIdAndSimulationId(@Param("deviceId") Long deviceId, @Param("userId") Long userId, @Param("simulationId") Long simulationId);

    @Transactional
    @Modifying
    @Query("delete from AttributeAmount a where a.deviceId = :deviceId and a.attributeId = :attributeId")
    void deleteByDeviceIdAndAttributeId(Long deviceId, Long attributeId);

    @Query("""
            select a.attributeId, a.deviceId, a.simulationId, a.userId, a.startingValue,
            at.name, at.description, at.type, at.simulationFunction, at.owner.id, at.isPrivate
             from AttributeAmount a
            left join AttributeTemplate at on a.attributeId = at.id
            where a.deviceId = :deviceId and a.userId = :userId and a.simulationId = :simulationId""")
    List<AttributeInfoResponse> findAllAttributesByIds(@Param("deviceId") Long deviceId, @Param("userId") Long userId, @Param("simulationId") Long simulationId);







}
