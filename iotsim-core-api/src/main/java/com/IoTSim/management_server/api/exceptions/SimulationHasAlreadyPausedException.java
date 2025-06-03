package com.IoTSim.management_server.api.exceptions;

public class SimulationHasAlreadyPausedException extends RuntimeException{
    public SimulationHasAlreadyPausedException() {
        super(ExceptionErrorText.SIMULATION_HAS_ALREADY_PAUSED.name());
    }
}
