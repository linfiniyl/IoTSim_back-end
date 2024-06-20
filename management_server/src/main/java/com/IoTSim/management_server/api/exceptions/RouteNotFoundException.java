package com.IoTSim.management_server.api.exceptions;


public class RouteNotFoundException extends RuntimeException{
    public RouteNotFoundException() {
        super(ExceptionErrorText.ROUTE_NOT_FOUND.name());
    }
}
