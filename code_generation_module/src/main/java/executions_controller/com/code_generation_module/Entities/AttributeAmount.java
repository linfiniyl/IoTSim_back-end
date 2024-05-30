package executions_controller.com.code_generation_module.Entities;


import jakarta.persistence.*;
import jakarta.persistence.Entity;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "attribute_amount")
@IdClass(AttributeAmountId.class)
public class AttributeAmount {
    @Id
    @Column(name = "entity_id", insertable=false, updatable=false)
    private Long entityId;
    @Id
    @Column(name = "attribute_id", insertable=false, updatable=false)
    private Long attributeId;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "entity_id", referencedColumnName = "id")
    private executions_controller.com.code_generation_module.Entities.Entity entity;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "attribute_id", referencedColumnName = "id")
    private Attribute attribute;

    public AttributeAmount(Long entityId, Long attributeId) {
        this.entityId = entityId;
        this.attributeId = attributeId;
    }

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

    public AttributeAmount(){}
}
