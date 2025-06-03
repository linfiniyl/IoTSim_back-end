package com.IoTSim.management_server.context.util;

import com.IoTSim.management_server.api.exceptions.*;
import com.IoTSim.management_server.context.attribute.repository.AttributeAmountRepository;
import com.IoTSim.management_server.context.attribute.repository.AttributeRelationRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;

import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.util.ObjectUtils;

import java.util.function.Supplier;


public class ContextUtils {

    public static ApplicationContext context = ApplicationContextHolder.getApplicationContext();

    public static User getCurrentUser() {
        UserRepository repository = context.getBean(UserRepository.class);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        return repository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
    }

    public static  <T> Supplier<? extends RuntimeException> throwEntityNotFoundException(Class<T> entityType) {
        return switch (entityType.getSimpleName()) {
            case Models.USER -> UserNotFoundException::new;
            case Models.ATTRIBUTE_TEMPLATE -> AttributeNotFoundException::new;
            case Models.DEVICE -> DeviceNotFoundException::new;
            case Models.ROUTE -> RouteNotFoundException::new;
            case Models.SIMULATION -> SimulationNotFoundException::new;
            default -> RuntimeException::new;
        };
    }

    public static void checkRelationExist(Long attributeId, Long deviceId, Logger log) {
        if (!context.getBean(AttributeRelationRepository.class).existsById(attributeId, deviceId)){
            log.error("Relation device not found for attribute: {}", attributeId);
            throw new RelationDeviceException();
        }
    }

    public static void checkAttributeAmountRelation(
            Long attributeId,
            Long deviceId,
            Long simulationId,
            Long userId,
            Logger log)
    {
        if (!context.getBean(AttributeAmountRepository.class).existsById(deviceId, attributeId, userId, simulationId)){
            log.error(
                    "Relation device not found for attribute amount with ID: {}, {}, {}, {}",
                    deviceId,
                    attributeId,
                    userId,
                    simulationId
            );
            throw new RelationDeviceException();
        }
    }

    public static class Models {
        public static final String USER = "User";
        public static final String ATTRIBUTE_TEMPLATE = "AttributeTemplate";
        public static final String DEVICE = "Device";
        public static final String SIMULATION = "Simulation";
        public static final String ROUTE = "Route";
    }
}
