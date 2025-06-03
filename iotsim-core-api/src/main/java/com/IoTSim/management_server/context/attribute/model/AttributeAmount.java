package com.IoTSim.management_server.context.attribute.model;


import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.user.model.User;
import jakarta.persistence.*;
import jakarta.persistence.Entity;
import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(
        name = "attribute_amount",
        uniqueConstraints = @UniqueConstraint(columnNames = {"attribute_relation_id", "user_id", "simulation_id"})
)
public class AttributeAmount {
    @Id
    @GeneratedValue
    private Long id;
    @Column(name = "attribute_relation_id", insertable=false, updatable=false)
    private Long attributeRelationId;
    @Column(name = "user_id", insertable=false, updatable=false)
    private Long userId;
    @Column(name = "simulation_id", insertable=false, updatable=false)
    private Long simulationId;
    @Column(name = "starting_value", nullable = false)
    private Long startingValue;

    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "user_id",  referencedColumnName = "id")
    private User user;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "attribute_relation_id", referencedColumnName = "id")
    private AttributeRelation relation;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "simulation_id", referencedColumnName = "id")
    private Simulation simulation;
}
