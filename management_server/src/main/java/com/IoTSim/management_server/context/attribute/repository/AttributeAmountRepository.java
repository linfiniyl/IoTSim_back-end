package com.IoTSim.management_server.context.attribute.repository;

import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeAmountId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AttributeAmountRepository extends JpaRepository<AttributeAmount, AttributeAmountId> {
    @Query(value = "SELECT * FROM attribute_amount WHERE device_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByDeviceId(Long deviceId);

    @Query(value = "SELECT * FROM attribute_amount WHERE attribute_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByAttributeId(Long attributeId);

    @Query("select (count(a) > 0) from AttributeAmount a where a.deviceId = ?1 and a.attributeId = ?2")
    boolean existByDeviceIdAndAttributeId(Long deviceId, Long attributeId);

    @Modifying
    @Query(value = "DELETE FROM attribute_amount WHERE device_id = ?1 and attribute_id = ?2" , nativeQuery = true)
    void deleteByDeviceIdAndAttributeId(Long deviceId, Long attributeId);

    @Query("select a from AttributeAmount a where a.deviceId = ?1 and a.attributeId = ?2")
    Optional<AttributeAmount> findByDeviceIdAndAttributeId(Long deviceId, Long attributeId);


}
