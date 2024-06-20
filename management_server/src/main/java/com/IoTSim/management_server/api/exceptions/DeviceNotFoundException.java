package com.IoTSim.management_server.api.exceptions;

import java.util.function.Supplier;

public class DeviceNotFoundException extends RuntimeException{
    public DeviceNotFoundException() {
        super(ExceptionErrorText.DEVICE_NOT_FOUND.name());
    }

}
