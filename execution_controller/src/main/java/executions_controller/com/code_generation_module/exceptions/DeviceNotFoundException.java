package executions_controller.com.code_generation_module.exceptions;

public class DeviceNotFoundException extends RuntimeException{
    public DeviceNotFoundException() {
        super(ExceptionErrorText.DEVICE_NOT_FOUND.name());
    }
}
