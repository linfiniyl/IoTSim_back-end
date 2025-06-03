package com.IoTSim.management_server.api.exceptions;

public class AttributeNotFoundException extends RuntimeException{
    public AttributeNotFoundException() {
        super(ExceptionErrorText.ATTRIBUTE_NOT_FOUND.name());
    }
}
