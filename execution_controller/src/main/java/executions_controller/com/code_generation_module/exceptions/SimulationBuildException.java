package executions_controller.com.code_generation_module.exceptions;

public class SimulationBuildException extends RuntimeException{
    public SimulationBuildException() {
        super(ExceptionErrorText.SIMULATION_BUILD_FAILED.name());
    }
}
