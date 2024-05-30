package executions_controller.com.code_generation_module.Entities;

import java.io.Serializable;


public class SimulationId implements Serializable {
    private Long userId;
    private Long id;

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getUserId() {
        return userId;
    }

    public Long getId() {
        return id;
    }

    public SimulationId(Long userId, Long id) {
        this.userId = userId;
        this.id = id;
    }

    public SimulationId() {
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SimulationId that = (SimulationId) o;

        if (!userId.equals(that.userId)) return false;
        return id.equals(that.id);
    }

    @Override
    public int hashCode() {
        int result = userId.hashCode();
        result = 31 * result + id.hashCode();
        return result;
    }
}
