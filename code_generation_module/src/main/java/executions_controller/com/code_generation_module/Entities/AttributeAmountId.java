package executions_controller.com.code_generation_module.Entities;


import java.io.Serializable;

public class AttributeAmountId implements Serializable {
    private Long entityId;

    private Long attributeId;

    public Long getEntityId() {
        return entityId;
    }

    public void setEntityId(Long entityId) {
        this.entityId = entityId;
    }

    public Long getAttributeId() {
        return attributeId;
    }

    public void setAttributeId(Long attributeId) {
        this.attributeId = attributeId;
    }

    public AttributeAmountId(Long entityId, Long attributeId) {
        this.entityId = entityId;
        this.attributeId = attributeId;
    }
    public AttributeAmountId(){}

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        AttributeAmountId that = (AttributeAmountId) o;

        if (!entityId.equals(that.entityId)) return false;
        return attributeId.equals(that.attributeId);
    }

    @Override
    public int hashCode() {
        int result = entityId.hashCode();
        result = 31 * result + attributeId.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "AttributeAmountId{" +
                "entityId=" + entityId +
                ", attributeId=" + attributeId +
                '}';
    }
}
