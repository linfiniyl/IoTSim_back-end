package com.IoTSim.management_server.api.exceptions;

public class DeviceWithoutAttributesException extends RuntimeException{
    public DeviceWithoutAttributesException() {
        super(ExceptionErrorText.DEVICE_WITHOUT_ATTRIBUTES.name());
    }
}
