package com.IoTSim.management_server.api.exceptions;

public class SimulationHasAlreadyStartedException extends RuntimeException{
    public SimulationHasAlreadyStartedException() {
        super(ExceptionErrorText.SIMULATION_HAS_ALREADY_STARTED.name());
    }
}
