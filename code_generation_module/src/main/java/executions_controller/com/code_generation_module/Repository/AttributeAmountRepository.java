package executions_controller.com.code_generation_module.Repository;

import executions_controller.com.code_generation_module.Entities.AttributeAmount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface AttributeAmountRepository extends JpaRepository<AttributeAmount, Long> {
    @Query(value = "SELECT * FROM attribute_amount WHERE entity_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByEntityId(Long entityId);

    @Query(value = "SELECT * FROM attribute_amount WHERE attribute_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByAttributeId(Long attributeId);

    @Query(value = "DELETE FROM attribute_amount WHERE entity_id = ?1 and attribute_id = ?2" , nativeQuery = true)
    void deleteByEntityId(Long entityId, Long attributeId);
}
