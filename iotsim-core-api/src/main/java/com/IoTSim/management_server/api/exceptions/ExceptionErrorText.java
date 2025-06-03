package com.IoTSim.management_server.api.exceptions;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum ExceptionErrorText {
    ATTRIBUTE_NOT_FOUND("Attribute Not Found"),
    DEVICE_NOT_FOUND("Device Not Found"),
    RELATION_DEVICE_EXCEPTION("Relation With Device Not Found"),
    ROUTE_NOT_FOUND("Route Not Found"),
    SIMULATION_NOT_FOUND("Simulation Not Found"),
    SIMULATION_HAS_ALREADY_STOPPED("Simulation Has Already Stopped"),
    SIMULATION_HAS_ALREADY_STARTED("Simulation Has Already Started"),
    SIMULATION_IS_ALREADY_RUNNING("Simulation Is Already Running"),
    SIMULATION_HAS_ALREADY_PAUSED("Simulation Has Already Paused"),
    DEVICE_WITHOUT_ATTRIBUTES("This Device Does Not Contain Any Attribute"),
    DEVICE_WITH_UNDEFINED_ATTRIBUTE("This Device Contains An Undefined Attribute"),
    SIMULATION_WITHOUT_DEVICES("This Simulation Does Not Contain Any Devices"),
    USER_NOT_FOUND("User Not Found");

    private final String message;
}
