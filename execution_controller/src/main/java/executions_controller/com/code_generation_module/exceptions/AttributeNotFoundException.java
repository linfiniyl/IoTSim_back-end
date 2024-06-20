package executions_controller.com.code_generation_module.exceptions;

public class AttributeNotFoundException extends RuntimeException{
    public AttributeNotFoundException() {
        super(ExceptionErrorText.ATTRIBUTE_NOT_FOUND.name());
    }
}
