package com.IoTSim.management_server.api.exceptions;

public class UserNotFoundException extends RuntimeException{
    public UserNotFoundException() {
        super(ExceptionErrorText.USER_NOT_FOUND.name());
    }
}
