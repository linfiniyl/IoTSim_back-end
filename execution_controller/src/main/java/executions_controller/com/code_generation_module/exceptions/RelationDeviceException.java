package executions_controller.com.code_generation_module.exceptions;


public class RelationDeviceException extends RuntimeException{
    public RelationDeviceException() {
        super(ExceptionErrorText.RELATION_DEVICE_EXCEPTION.name());
    }
}
