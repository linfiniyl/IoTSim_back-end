package com.IoTSim.management_server.api.exceptions;

public class SimulationWithoutDevicesException extends RuntimeException{
    public SimulationWithoutDevicesException() {
        super(ExceptionErrorText.SIMULATION_WITHOUT_DEVICES.name());
    }
}
