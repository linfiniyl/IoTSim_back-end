package com.IoTSim.management_server.api.exceptions;


public class RelationDeviceException extends RuntimeException{
    public RelationDeviceException() {
        super(ExceptionErrorText.RELATION_DEVICE_EXCEPTION.name());
    }
}
