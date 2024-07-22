package com.IoTSim.management_server.rabbitmq.service;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.user.model.User;

public interface CommandService {
    void startSimulation(Long simulationId);
    void stopSimulation(Long simulationId);
    //void restartSimulation(Long simulationId);
    void createSimulationProcesses(Simulation simulation, User user);

    void pauseSimulation(Long simulationId);
    void continueSimulation(Long simulationId);
}
