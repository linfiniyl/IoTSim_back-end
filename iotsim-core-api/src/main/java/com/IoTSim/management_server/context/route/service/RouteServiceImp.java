package com.IoTSim.management_server.context.route.service;

import com.IoTSim.management_server.api.exceptions.RouteNotFoundException;
import com.IoTSim.management_server.api.exceptions.UserNotFoundException;
import com.IoTSim.management_server.context.route.api.CreateRouteRequest;
import com.IoTSim.management_server.context.route.dto.RouteDto;
import com.IoTSim.management_server.context.route.dto.RoutePointDto;
import com.IoTSim.management_server.context.route.mapper.RouteMapper;
import com.IoTSim.management_server.context.route.model.Route;
import com.IoTSim.management_server.context.route.repository.RoutePointRepository;
import com.IoTSim.management_server.context.route.repository.RouteRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Objects;


@Slf4j
@Service
@RequiredArgsConstructor
public class RouteServiceImp implements RouteService {

    private final RouteRepository routeRepository;
    private final RoutePointRepository routePointRepository;
    private final UserRepository userRepository;
    private final RouteMapper mapper;

    @Override
    @Transactional
    public List<RouteDto> getAllRoutesByUser() {
        log.debug("Fetching all routes for the user.");
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while fetching routes.");
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        List<RouteDto> routes = mapper.routeListToRouteDtoList(routeRepository.findByOwnerAndIsPrivateFalse(user));
        log.info("Fetched {} routes for user ID: {}", routes.size(), user.getId());
        return routes;
    }

    @Override
    @Transactional
    public RouteDto getRouteById(Long routeId) {
        log.debug("Fetching route with ID: {}", routeId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while fetching route by ID.");
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        RouteDto routeDto = mapper.routeToRouteDto(routeRepository.findById(routeId).orElseThrow(RouteNotFoundException::new));
        log.info("Fetched route ID: {} for user ID: {}", routeId, user.getId());
        return routeDto;
    }

    @Override
    @Transactional
    public void deleteRouteById(
            Long routeId
    ) {
        log.debug("Attempting to delete route with ID: {}", routeId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while attempting to delete route.");
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!routeRepository.existsById(routeId)) {
            log.error("Route with ID: {} not found for deletion.", routeId);
            throw new RouteNotFoundException();
        }
        routeRepository.deleteById(routeId);
        log.info("Deleted route ID: {} by user ID: {}", routeId, user.getId());
    }

    @Override
    @Transactional
    public void updateRoute(RouteDto routeDto) {
        log.debug("Updating route with ID: {}", routeDto.getId());
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while attempting to update route.");
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!routeRepository.existsById(routeDto.getId())) {
            log.error("Route with ID: {} not found for update.", routeDto.getId());
            throw new RouteNotFoundException();
        }
        routeRepository.save(mapper.routeDtoToRoute(routeDto));
        log.info("Updated route ID: {} by user ID: {}", routeDto.getId(), user.getId());
    }

    @Override
    @Transactional
    public RouteDto createRoute(CreateRouteRequest request) {
        log.debug("Creating a new route with name: {}", request.getName());
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while attempting to create a route.");
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        Route route = Route.builder()
                .name(request.getName())
                .isPrivate(request.getIsPrivate())
                .routePoints(request.getRoutePointSet())
                .owner(user)
                .build();
        RouteDto routeDto = mapper.routeToRouteDto(routeRepository.saveAndFlush(route));
        log.info("Created route ID: {} by user ID: {}", routeDto.getId(), user.getId());
        return routeDto;
    }

    @Override
    @Transactional
    public List<RoutePointDto> getAllPointByRouteId(Long routeId) {
        log.debug("Fetching all points for route ID: {}", routeId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while fetching route points.");
            throw new UserNotFoundException();
        }
        Route route = routeRepository.findById(routeId).orElseThrow(RouteNotFoundException::new);
        User owner = route.getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(owner.getId(), user.getId()) && route.getIsPrivate()) {
            log.error("Access denied for user ID: {} to route ID: {}", user.getId(), routeId);
            throw new AccessDeniedException("Access Denied");
        }

        List<RoutePointDto> routePoints = mapper.routePointListToRoutePointDtoList(routePointRepository.findByRouteId(routeId));
        log.info("Fetched {} points for route ID: {}", routePoints.size(), routeId);
        return routePoints;
    }

    @Override
    @Transactional
    public void updatePointsByRoute(List<RoutePointDto> routePointDtoList, Long routeId) {
        log.debug("Updating points for route ID: {}", routeId);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)) {
            log.error("User not found while attempting to update route points.");
            throw new UserNotFoundException();
        }
        Route route = routeRepository.findById(routeId).orElseThrow(RouteNotFoundException::new);
        User owner = route.getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(owner.getId(), user.getId()) && route.getIsPrivate()) {
            log.error("Access denied for user ID: {} to update points for route ID: {}", user.getId(), routeId);
            throw new AccessDeniedException("Access Denied");
        }

        routePointDtoList.stream()
                .map(mapper::routePointDtoToRoutePoint)
                .map(routePointRepository::save)
                .toList();
        log.info("Updated points for route ID: {} by user ID: {}", routeId, user.getId());
    }
}
