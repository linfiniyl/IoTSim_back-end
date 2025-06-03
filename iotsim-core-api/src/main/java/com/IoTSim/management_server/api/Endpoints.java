package com.IoTSim.management_server.api;

public interface Endpoints {
    String AUTH = "/api/v1/auth";
    String LOGIN = "/login";
    String REGISTER = "/register";
    String GET_ACCESS_TOKEN="/token";
    String REFRESH ="/refresh";
    String ATTRIBUTE_TEMPLATES = "/api/v1/attributeTemplates";
    String DEVICE_RELATIONS_AMOUNT = "/api/v1/deviceRelations";
    String DEVICE_RELATION_ALL_ID_AMOUNT = "/{deviceId}/{userId}/{simulationId}";
    String ATTRIBUTE_TEMPLATE_ID = "/{attributeId}";
    String DEVICE_RELATION_ID_AMOUNT = "/{deviceId}/{userId}/{simulationId}/{attributeId}";
    String SIMULATION_RELATIONS = "/api/v1/simulationRelations";
    String SIMULATION_RELATIONS_SIMULATION_ID = "/{simulationId}";
    String DEVICE_RELATION_ATTRIBUTES = "api/v1/deviceAttributeRelations";
    String DEVICE_RELATION_ATTRIBUTE_ID = "/{deviceId}/{attributeId}";

    String DEVICE_RELATION_ATTRIBUTE_DEVICE_ID = "/{deviceId}";

    String SIMULATION_RELATION_ID = "/{simulationId}/{deviceId}";
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

    String MINIO =  "/api/v1/files";

    String UPLOAD_FILE ="/upload";
    String DOWNLOAD_FILE ="/download/{filename}";
}
