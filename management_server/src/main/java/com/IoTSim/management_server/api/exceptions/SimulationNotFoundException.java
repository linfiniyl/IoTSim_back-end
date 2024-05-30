package com.IoTSim.management_server.api.exceptions;

public class SimulationNotFoundException extends RuntimeException{
    public SimulationNotFoundException(String message) {
        super(message);
    }
}
