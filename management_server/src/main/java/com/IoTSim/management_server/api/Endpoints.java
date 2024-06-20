package com.IoTSim.management_server.api;

public interface Endpoints {
    String AUTH = "/auth";
    String LOGIN = "/login";
    String REGISTER = "/register";
    String ATTRIBUTE_TEMPLATES = "/api/v1/attributeTemplates";
    String DEVICE_RELATIONS = "/api/v1/deviceRelations/{deviceId}";
    String ATTRIBUTE_TEMPLATE_ID = "/{attributeId}";
    String DEVICE_RELATION_ID = "/{attributeId}";
    String SIMULATION_RELATIONS = "/api/v1/simulationRelations/{simulationId}";
    String SIMULATION_RELATION_ID = "/{deviceId}";
    String USERS = "/api/v1/users";
    String USER_ID = "/{id}";
    String SIMULATIONS = "/api/v1/simulations";
    String SIMULATION_ID = "/{id}";
    String DEVICES = "/api/v1/devices";
    String DEVICES_ID = "/{deviceId}";

    String SIMULATION_OPERATION = "/api/v1/operations";
    String OPERATION_START = "/start/{simulationId}";
    String OPERATION_STOP = "/stop/{simulationId}";
    String OPERATION_RESTART = "/restart/{simulationId}";
    String ROUTES = "/api/v1/routes";
    String ROUTE_ID = "/{routeId}";
    String ROUTE_POINT_ID = "/{routeId}/points";
}
