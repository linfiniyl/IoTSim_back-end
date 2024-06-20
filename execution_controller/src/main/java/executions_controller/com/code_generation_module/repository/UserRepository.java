package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    Optional<User> findByEmail(String email);

    @Query("select (count(u) > 0) from User u where u.id = ?1")
    boolean existById(long id);
}
