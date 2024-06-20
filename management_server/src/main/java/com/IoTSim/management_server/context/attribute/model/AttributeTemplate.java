package com.IoTSim.management_server.context.attribute.model;

import com.IoTSim.management_server.context.user.model.User;
import jakarta.persistence.Entity;
import jakarta.persistence.*;
import lombok.*;

import java.util.LinkedHashSet;
import java.util.Set;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Builder
@Table(name = "attribute")
public class AttributeTemplate {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "name")
    private String name;
    @Column(name = "description")
    private String description;
    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private AttributeType type;
    @Enumerated(EnumType.STRING)
    @Column(name = "simulation_function")
    private SimulationFunctions simulationFunctions;
    @Column(name = "is_private")
    private Boolean isPrivate;
    @OneToMany(mappedBy = "attributeTemplate", orphanRemoval = true)
    private Set<AttributeAmount> devices = new LinkedHashSet<>();
    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id")
    private User owner;

}
