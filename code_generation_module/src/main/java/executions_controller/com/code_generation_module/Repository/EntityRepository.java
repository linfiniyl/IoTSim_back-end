package executions_controller.com.code_generation_module.Repository;

import executions_controller.com.code_generation_module.Entities.Entity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
@Repository
public interface EntityRepository extends JpaRepository<Entity, Long> {
}
