package com.IoTSim.management_server.api.exceptions;

public class SimulationIsAlreadyRunningException extends RuntimeException{
    public SimulationIsAlreadyRunningException() {
        super(ExceptionErrorText.SIMULATION_IS_ALREADY_RUNNING.name());
    }
}
