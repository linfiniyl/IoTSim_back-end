package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.Route;
import executions_controller.com.code_generation_module.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Optional;

public interface RouteRepository extends JpaRepository<Route, Long> {
    List<Route> findByOwnerAndIsPrivateFalse(User owner);


    @NonNull
    @Override
    Optional<Route> findById(@NonNull Long routeId);

    @Override
    boolean existsById(@NonNull Long routeId);


}
