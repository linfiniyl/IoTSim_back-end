package executions_controller.com.code_generation_module.exceptions;

public class UserNotFoundException extends RuntimeException{
    public UserNotFoundException() {
        super(ExceptionErrorText.USER_NOT_FOUND.name());
    }
}
