package com.IoTSim.management_server.context.attribute.repository;


import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface AttributeTemplateRepository extends JpaRepository<AttributeTemplate, Long> {
    @NonNull
    @Override
    Optional<AttributeTemplate> findById(@NonNull Long id);

    boolean existsById(@NonNull Long id);

    @Query("select a from AttributeTemplate a where a.owner.id = ?1")
    List<AttributeTemplate> findAllAttributesTemplateByOwner(long id);

    @Query("select a from AttributeTemplate a where a.owner.id = ?1 and a.isPrivate = false")
    List<AttributeTemplate> findAllAttributesByOwnerIdAndIsPrivateFalse(long userId);


    @Query("select a from AttributeTemplate a inner join a.devices devices where devices.deviceId = ?1")
    List<AttributeTemplate> findAllByDeviceId(Long deviceId);

    @Query("""
            select a from AttributeTemplate a inner join a.devices devices
            where devices.deviceId = :deviceId and devices.userId = :userId and devices.simulationId = :simulationId""")
    List<AttributeTemplate> findByDevicesDeviceIdAndDevicesUserIdAndDevicesSimulationId(@Param("deviceId") Long deviceId, @Param("userId") Long userId, @Param("simulationId") Long simulationId);






}
