package com.IoTSim.management_server.context.util;


import com.IoTSim.management_server.context.user.model.User;
import org.springframework.security.access.AccessDeniedException;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class AccessCheckUtils {

    public static AccessParametersHolder AccessForUser(User user){
        return new AccessParametersHolder(user);
    }

    public static class AccessParametersHolder {
        private final User user;
        private final Map<Class<? extends AccessCheckable>, Long> models;

        public AccessParametersHolder(User user){
            this.user = user;
            this.models = new HashMap<>();
        }

        public AccessParametersHolder withModel(Long id, Class<? extends AccessCheckable> repositoryClass) {
            this.models.put(repositoryClass, id);
            return this;
        }

        public void check(){
            Optional<Boolean> result = models.entrySet()
                    .stream()
                    .map(entry -> RepositoryUtils.findObjectById(entry.getKey(), entry.getValue()))
                    .map(obj -> obj.hasAccess(user))
                    .filter(success -> success)
                    .findFirst();
            if (result.isEmpty())
                throw new AccessDeniedException("Access Denied");
        }
    }
}
