package com.IoTSim.management_server.api.exceptions;

public class SimulationHasAlreadyStoppedException extends RuntimeException{
    public SimulationHasAlreadyStoppedException() {
        super(ExceptionErrorText.SIMULATION_HAS_ALREADY_STOPPED.name());
    }
}
