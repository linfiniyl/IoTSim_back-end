package com.IoTSim.management_server.context.simulation.model;

import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.route.model.Route;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.device.model.DevicesAmount;
import com.IoTSim.management_server.context.util.AccessCheckable;
import jakarta.persistence.*;
import jakarta.persistence.Entity;
import lombok.*;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "simulation")
public class Simulation implements AccessCheckable {
    @Id
    @GeneratedValue
    @Column(name = "id", nullable = false)
    private Long id;
    @Column(name = "name", nullable = false)
    private String name;
    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    private Status simulationStatus;
    @Column(name = "is_private")
    private Boolean isPrivate = true;
    @OneToMany(mappedBy = "simulation", orphanRemoval = true)
    private Set<DevicesAmount> amount = new LinkedHashSet<>();
    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route route;

    @OneToMany(mappedBy = "simulation", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AttributeAmount> attributeAmounts = new LinkedHashSet<>();

    @Override
    public boolean hasAccess(User user) {
        return Objects.equals(user.getId(), this.user.getId()) || !isPrivate;
    }
}
