package com.IoTSim.management_server.context.route.model;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.user.model.User;
import jakarta.persistence.*;
import lombok.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.LinkedHashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "route")
public class Route {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "route_name", nullable = false)
    private String name;
    @Column(name = "is_private", nullable = false)
    private Boolean isPrivate;

    @OneToMany(mappedBy = "route", orphanRemoval = true)
    private Set<RoutePoint> routePoints = new LinkedHashSet<>();


    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "owner_id", nullable = false)
    private User owner;

    @OneToMany(mappedBy = "route", cascade = CascadeType.ALL)
    private Set<Simulation> simulations = new LinkedHashSet<>();

}
