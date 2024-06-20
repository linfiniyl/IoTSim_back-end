package executions_controller.com.code_generation_module.repository;


import executions_controller.com.code_generation_module.entities.AttributeTemplate;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;


@Repository
public interface AttributeTemplateRepository extends JpaRepository<AttributeTemplate, Long> {

    Optional<AttributeTemplate> findById(Long id);

    boolean existsById(Long id);

    @Query("select a from AttributeTemplate a where a.owner.id = ?1")
    List<AttributeTemplate> findAllAttributesTemplateByOwner(long id);

    @Query("select a from AttributeTemplate a where a.owner.id = ?1 and a.isPrivate = false")
    List<AttributeTemplate> findAllAttributesByOwnerIdAndIsPrivateFalse(long userId);


    @Query("select a from AttributeTemplate a inner join a.devices devices where devices.deviceId = ?1")
    List<AttributeTemplate> findAllByDeviceId(Long deviceId);



}
