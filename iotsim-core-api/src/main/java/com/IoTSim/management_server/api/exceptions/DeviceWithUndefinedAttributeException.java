package com.IoTSim.management_server.api.exceptions;

public class DeviceWithUndefinedAttributeException extends RuntimeException{
    public DeviceWithUndefinedAttributeException() {
        super(ExceptionErrorText.DEVICE_WITH_UNDEFINED_ATTRIBUTE.name());
    }
}
