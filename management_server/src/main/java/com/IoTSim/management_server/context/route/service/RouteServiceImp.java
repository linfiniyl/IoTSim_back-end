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
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class RouteServiceImp implements RouteService{

    private final RouteRepository routeRepository;
    private final RoutePointRepository routePointRepository;
    private final UserRepository userRepository;
    private final RouteMapper mapper;

    @Override
    @Transactional
    public List<RouteDto> getAllRoutesByUser() {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        return mapper.routeListToRouteDtoList(
                routeRepository.findByOwnerAndIsPrivateFalse(user)
        );
    }

    @Override
    @Transactional
    public RouteDto getRouteById(
            Long routeId
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        return mapper.routeToRouteDto(
                routeRepository.findById(routeId).orElseThrow(RouteNotFoundException::new)
        );
    }

    @Override
    @Transactional
    public void deleteRouteById(
            Long routeId
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!routeRepository.existsById(routeId)){
            throw new RouteNotFoundException();
        }
        routeRepository.deleteById(routeId);
    }

    @Override
    @Transactional
    public void updateRoute(
            RouteDto routeDto
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);

        if (!routeRepository.existsById(routeDto.getId())){
            throw new RouteNotFoundException();
        }
        routeRepository.save(
                mapper.routeDtoToRoute(routeDto)
        );
    }

    @Override
    @Transactional
    public RouteDto createRoute(CreateRouteRequest request) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        Route route = Route
                .builder()
                .name(request.getName())
                .isPrivate(request.getIsPrivate())
                .routePoints(request.getRoutePointSet())
                .owner(user)
                .build();
        return mapper.routeToRouteDto(
                routeRepository.saveAndFlush(route)
        );
    }

    @Override
    @Transactional
    public List<RoutePointDto> getAllPointByRouteId(Long routeId) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Route route = routeRepository.findById(routeId).orElseThrow(RouteNotFoundException::new);
        User owner = route.getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(owner.getId(), user.getId()) && route.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }

        return mapper.routePointListToRoutePointDtoList(
                routePointRepository.findByRouteId(routeId)
        );
    }

    @Override
    @Transactional
    public void updatePointsByRoute(
            List<RoutePointDto> routePointDtoList,
            Long routeId
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Route route = routeRepository.findById(routeId).orElseThrow(RouteNotFoundException::new);
        User owner = route.getOwner();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(owner.getId(), user.getId()) && route.getIsPrivate()){
            throw new AccessDeniedException("Access Denied");
        }

        routePointDtoList.stream()
                .map(mapper::routePointDtoToRoutePoint)
                .map(routePointRepository::save)
                .toList();
    }
}
