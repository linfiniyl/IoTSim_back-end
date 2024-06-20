package executions_controller.com.code_generation_module.exceptions;

public class SimulationNotFoundException extends RuntimeException{
    public SimulationNotFoundException() {
        super(ExceptionErrorText.SIMULATION_NOT_FOUND.name());
    }
}
