package com.IoTSim.management_server.api.exceptions;

public class SimulationNotFoundException extends RuntimeException{
    public SimulationNotFoundException() {
        super(ExceptionErrorText.SIMULATION_NOT_FOUND.name());
    }
}
