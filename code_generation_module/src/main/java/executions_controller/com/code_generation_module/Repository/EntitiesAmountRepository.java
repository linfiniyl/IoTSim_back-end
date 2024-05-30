package executions_controller.com.code_generation_module.Repository;

import executions_controller.com.code_generation_module.Entities.EntitiesAmount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface EntitiesAmountRepository extends JpaRepository<EntitiesAmount,Long> {
    @Query(value = "SELECT * FROM entities_amount WHERE user_id = ?1 and simulation_id = ?2" , nativeQuery = true)
    List<EntitiesAmount> findAllEntitiesById(Long userId, Long simulation_id);

    @Query(value = "DELETE FROM entities_amount WHERE user_id = ?1 and simulation_id = ?2 and entity_id = ?3", nativeQuery = true)
    void deleteEntitiesAmounById(Long userId, Long id, Long entityId);

    @Query(value = "SELECT * FROM entities_amount " +
            "WHERE user_id = ?1 and simulation_id = ?2 and entity_id = ?3" , nativeQuery = true)
    Optional<EntitiesAmount> findEntityById(Long userId, Long simulation_id, Long entityId);
}
