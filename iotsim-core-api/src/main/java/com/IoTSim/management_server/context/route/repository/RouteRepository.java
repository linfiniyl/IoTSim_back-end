package com.IoTSim.management_server.context.route.repository;

import com.IoTSim.management_server.context.route.model.Route;
import com.IoTSim.management_server.context.user.model.User;
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
