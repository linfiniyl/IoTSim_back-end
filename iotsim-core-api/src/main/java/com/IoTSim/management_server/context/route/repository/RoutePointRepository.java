package com.IoTSim.management_server.context.route.repository;

import com.IoTSim.management_server.context.route.model.RoutePoint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface RoutePointRepository extends JpaRepository<RoutePoint, Long> {

    @Query("select r from RoutePoint r where r.route.id = ?1")
    List<RoutePoint> findByRouteId(Long id);


}
