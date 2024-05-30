package executions_controller.com.code_generation_module.Entities;

import jakarta.persistence.*;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@jakarta.persistence.Entity
@Table(name = "entities_amount")
@IdClass(EntitiesAmountId.class)
public class EntitiesAmount {

    @Id
    @Column(name = "user_id", insertable=false, updatable=false)
    private Long userId;

    @Id
    @Column(name = "simulation_id", insertable=false, updatable=false)
    private Long id;
    @Id
    @Column(name = "entity_id", insertable=false, updatable=false)
    private Long entityId;

    @Column(name = "amount_entities")
    private Long amount;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumns({
            @JoinColumn(name = "user_id", referencedColumnName = "user_id"),
            @JoinColumn(name = "simulation_id", referencedColumnName = "id")
    })
    private Simulation simulation;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "entity_id", referencedColumnName = "id")
    private Entity entity;

    public Long getUserId() {
        return userId;
    }

    public Long getId() {
        return id;
    }

    public Long getEntityId() {
        return entityId;
    }

    public Long getAmount() {
        return amount;
    }

    public Simulation getSimulation() {
        return simulation;
    }

    public Entity getEntity() {
        return entity;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setEntityId(Long entityId) {
        this.entityId = entityId;
    }

    public void setAmount(Long amount) {
        this.amount = amount;
    }

    public EntitiesAmount(Long userId, Long id, Long entityId, Long amount, Simulation simulation, Entity entity) {
        this.userId = userId;
        this.id = id;
        this.entityId = entityId;
        this.amount = amount;
        this.simulation = simulation;
        this.entity = entity;
    }

    public EntitiesAmount() {
    }
}
