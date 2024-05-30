package com.IoTSim.management_server.context.attribute.repository;

import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface AttributeTemplateRepository extends JpaRepository<AttributeTemplate, Long> {
    @Override
    Optional<AttributeTemplate> findById(Long id);

    boolean existsById(Long id);

    @Query("select a from AttributeTemplate a where a.owner.id = ?1")
    List<AttributeTemplate> findAllAttributeTemplateByOwner(long id);

    @Query("select a from AttributeTemplate a inner join a.entities entities where entities.entityId = ?1")
    List<AttributeTemplate> findAllByEntityId(Long entityId);



}
