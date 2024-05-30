package com.IoTSim.management_server.api.exceptions;

public class AttributeNotFoundException extends RuntimeException{
    public AttributeNotFoundException(String message) {
        super(message);
    }
}
