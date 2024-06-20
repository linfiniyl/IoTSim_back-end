package executions_controller.com.code_generation_module.service;

import executions_controller.com.code_generation_module.entities.Route;
import executions_controller.com.code_generation_module.entities.RoutePoint;
import executions_controller.com.code_generation_module.exceptions.RouteNotFoundException;
import executions_controller.com.code_generation_module.repository.RoutePointRepository;
import executions_controller.com.code_generation_module.repository.RouteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RouteService {

    private final RouteRepository routeRepository;
    private final RoutePointRepository routePointRepository;

    @Transactional
    public Route getRouteById(
            Long routeId
    ) {
        if (!routeRepository.existsById(routeId)){
            throw new RouteNotFoundException();
        }
        return routeRepository.findById(routeId).get();
    }


    @Transactional
    public List<RoutePoint> getAllPointByRouteId(Long routeId) {

        if (!routeRepository.existsById(routeId)){
            throw new RouteNotFoundException();
        }
        Route route = routeRepository.findById(routeId).get();

        return routePointRepository.findByRouteId(routeId);
    }
}
