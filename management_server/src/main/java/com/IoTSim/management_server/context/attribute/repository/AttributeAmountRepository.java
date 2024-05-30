package com.IoTSim.management_server.context.attribute.repository;

import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;


@Repository
public interface AttributeAmountRepository extends JpaRepository<AttributeAmount, Long> {
    @Query(value = "SELECT * FROM attribute_amount WHERE entity_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByEntityId(Long entityId);

    @Query(value = "SELECT * FROM attribute_amount WHERE entity_id = ?1 and attribute_id = ?2" , nativeQuery = true)
    List<AttributeAmount> findAllByAttributeId(Long attributeId);


    @Query("select (count(a) > 0) from AttributeAmount a where a.entityId = ?1 and a.attributeId = ?2")
    boolean existByEntityIdAndAttributeId(Long entityId, Long attributeId);

    @Query(value = "SELECT * FROM attribute_amount WHERE  attribute_id = ?1" , nativeQuery = true)
    Optional<AttributeAmount> findByEntityIdAndAttributeId(Long entityId, Long attributeId);
    @Modifying
    @Query(value = "DELETE FROM attribute_amount WHERE entity_id = ?1 and attribute_id = ?2" , nativeQuery = true)
    void deleteByEntityIdAndAttributeId(Long entityId, Long attributeId);

    @Transactional
    @Modifying
    @Query("delete from AttributeAmount a where a.attributeId = ?1")
    void deleteByAttributeId(Long attributeId);

    @Query("select (count(a) > 0) from AttributeAmount a where a.attributeId = ?1")
    boolean existByAttributeId(Long attributeId);

    @Query("select a from AttributeAmount a where a.entityId = ?1")
    List<AttributeAmount> findAllRelationsByEntityId(Long entityId);





}
