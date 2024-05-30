package executions_controller.com.code_generation_module.Repository;

import executions_controller.com.code_generation_module.Entities.Attribute;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;


@Repository
public interface AttributeRepository extends JpaRepository<Attribute, Long> {
}
