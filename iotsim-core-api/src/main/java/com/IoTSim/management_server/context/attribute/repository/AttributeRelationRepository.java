package com.IoTSim.management_server.context.attribute.repository;
import com.IoTSim.management_server.context.attribute.model.AttributeRelation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AttributeRelationRepository extends JpaRepository<AttributeRelation, Long> {
    @Query("select (count(a) > 0) from AttributeRelation a where a.attributeId = :attributeId and a.deviceId = :deviceId")
    boolean existsById(@Param("attributeId") Long attributeId, @Param("deviceId") Long deviceId);

    @Query("""
            select count(aa) 
            from AttributeRelation ar
            left join AttributeAmount aa
            on ar.id = aa.attributeRelationId
            where aa.startingValue is NULL
                and aa.simulationId = ?1
            """)
    long countNotDefinedAttributesBySimulationId(Long simulationId);

    void deleteAttributeRelationByAttributeIdAndDeviceId(Long attributeId, Long deviceId);
}
