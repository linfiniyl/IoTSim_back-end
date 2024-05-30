package executions_controller.com.code_generation_module.Repository;

import executions_controller.com.code_generation_module.Entities.Simulation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;


public interface SimulationRepository extends JpaRepository<Simulation, Long> {

    @Query(value = "SELECT * FROM simulations WHERE user_id = ?1" , nativeQuery = true)
    List<Simulation> findAllSimulationByUserId(Long userId);
    @Query(value = "SELECT * FROM simulations WHERE user_id = ?1 AND id = ?2" , nativeQuery = true)
    Optional<Simulation> findById(Long userId, Long id);
    @Query(value = "DELETE FROM simulations WHERE user_id = ?1 AND id = ?2" , nativeQuery = true)
    void deleteById(Long userId, Long id);


}
