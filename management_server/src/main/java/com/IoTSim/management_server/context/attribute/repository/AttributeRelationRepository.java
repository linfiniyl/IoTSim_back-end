package com.IoTSim.management_server.context.attribute.repository;

import com.IoTSim.management_server.context.attribute.model.AttributeRelation;
import com.IoTSim.management_server.context.attribute.model.AttributeRelationId;
import com.IoTSim.management_server.context.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface AttributeRelationRepository extends JpaRepository<AttributeRelation, AttributeRelationId> {
    @Query("select (count(a) > 0) from AttributeRelation a where a.attributeId = :attributeId and a.deviceId = :deviceId")
    boolean existsById(@Param("attributeId") Long attributeId, @Param("deviceId") Long deviceId);

    @Query("""
            select count(aa) from AttributeRelation ar
            left join AttributeAmount aa
            on ar.attributeId = aa.attributeId
            and ar.deviceId = aa.deviceId
            where aa.startingValue is NULL and aa.simulationId = ?1
            """)
    long countNotDefinedAttributesBySimulationId(Long simulationId);



}
