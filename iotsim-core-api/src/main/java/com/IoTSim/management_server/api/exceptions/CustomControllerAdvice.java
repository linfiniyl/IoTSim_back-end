package com.IoTSim.management_server.api.exceptions;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.nio.file.AccessDeniedException;

@ControllerAdvice
public class CustomControllerAdvice {
    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ExceptionHandler(value
            = {UserNotFoundException.class, AttributeNotFoundException.class,
            RelationDeviceException.class, DeviceNotFoundException.class,
            SimulationNotFoundException.class, RouteNotFoundException.class})
    public ResponseEntity<ErrorMessage> notFoundExceptionHandler(RuntimeException exception){
        return ResponseEntity
                .status(HttpStatus.NOT_FOUND)
                .body(new ErrorMessage(exception.getMessage()));
    }

    @ResponseStatus(HttpStatus.FORBIDDEN)
    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ErrorMessage> accessDeniedExceptionHandler(AccessDeniedException exception){
        return ResponseEntity
                .status(HttpStatus.FORBIDDEN)
                .body(new ErrorMessage(exception.getMessage()));
    }

    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    @ExceptionHandler(NullPointerException.class)
    public ResponseEntity<ErrorMessage> nullPointerExceptionHandler(NullPointerException exception){
        return ResponseEntity
                .status(HttpStatus.FORBIDDEN)
                .body(new ErrorMessage(exception.getMessage()));
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(value ={
            SimulationHasAlreadyPausedException.class, SimulationHasAlreadyStartedException.class,
            SimulationIsAlreadyRunningException.class, SimulationHasAlreadyStoppedException.class
    })
    public ResponseEntity<ErrorMessage> simulationCommandExceptionHandler(RuntimeException exception){
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(new ErrorMessage(exception.getMessage()));
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(value = {
            SimulationWithoutDevicesException.class, DeviceWithoutAttributesException.class,
            DeviceWithUndefinedAttributeException.class
    })
    public ResponseEntity<ErrorMessage> simulationStartingExceptionHandler(RuntimeException exception){
        return ResponseEntity
                .status(HttpStatus.BAD_REQUEST)
                .body(new ErrorMessage(exception.getMessage()));
    }
}
