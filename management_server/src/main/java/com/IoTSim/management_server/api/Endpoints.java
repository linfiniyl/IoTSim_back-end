package com.IoTSim.management_server.api;

public interface Endpoints {
    String AUTH = "/auth";
    String LOGIN = "/login";
    String REGISTER = "/register";
    String ALL_USERS_ATTRIBUTE_TEMPLATE = "/api/v1/attributesTemplate";
    String ALL_ENTITY_ATTRIBUTES = "/api/v1/{entityId}/attributes";
    String ATTRIBUTE_TEMPLATE = "/api/v1/attributesTemplate/{attributeId}";
    String ATTRIBUTE = "/api/v1/{entityId}/attributes/{attributeId}";
    String USERS = "/api/v1/users";
    String USER_ID = "/id";
}
