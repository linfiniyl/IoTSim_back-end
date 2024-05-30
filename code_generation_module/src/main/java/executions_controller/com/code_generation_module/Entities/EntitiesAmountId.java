package executions_controller.com.code_generation_module.Entities;


import java.io.Serializable;

public class EntitiesAmountId implements Serializable {

    private Long userId;
    private Long id;
    private Long entityId;

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

    public void setEntityId(Long entityId) {
        this.entityId = entityId;
    }

    public Long getEntityId() {
        return entityId;
    }

    public EntitiesAmountId(Long userId, Long id, Long entityId) {
        this.userId = userId;
        this.id = id;
        this.entityId = entityId;
    }

    public EntitiesAmountId() {
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EntitiesAmountId that = (EntitiesAmountId) o;

        if (!userId.equals(that.userId)) return false;
        if (!id.equals(that.id)) return false;
        return entityId.equals(that.entityId);
    }

    @Override
    public int hashCode() {
        int result = userId.hashCode();
        result = 31 * result + id.hashCode();
        result = 31 * result + entityId.hashCode();
        return result;
    }
}
