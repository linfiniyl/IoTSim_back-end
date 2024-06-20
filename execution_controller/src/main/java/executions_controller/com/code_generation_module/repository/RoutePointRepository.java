package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.RoutePoint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface RoutePointRepository extends JpaRepository<RoutePoint, Long> {

    @Query("select r from RoutePoint r where r.route.id = ?1")
    List<RoutePoint> findByRouteId(Long id);

    @Query("select r from RoutePoint r inner join r.route.simulations simulations where simulations.id = ?1")
    List<RoutePoint> findByRouteSimulationsId(Long simulationId);


}
