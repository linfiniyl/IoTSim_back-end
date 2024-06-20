package com.IoTSim.management_server.api.exceptions;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum ExceptionErrorText {
    ATTRIBUTE_NOT_FOUND("Attribute Not Found"),
    DEVICE_NOT_FOUND("Device Not Found"),
    RELATION_DEVICE_EXCEPTION("Relation With Device Not Found"),
    ROUTE_NOT_FOUND("Route Not Found"),
    SIMULATION_NOT_FOUND("Simulation Not Found"),
    USER_NOT_FOUND("User Not Found");

    private final String message;
}
