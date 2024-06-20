package executions_controller.com.code_generation_module.exceptions;


public class RouteNotFoundException extends RuntimeException{
    public RouteNotFoundException() {
        super(ExceptionErrorText.ROUTE_NOT_FOUND.name());
    }
}
