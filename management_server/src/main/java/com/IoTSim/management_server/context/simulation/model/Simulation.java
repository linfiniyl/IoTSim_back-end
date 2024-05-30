package com.IoTSim.management_server.context.simulation.model;

import com.IoTSim.management_server.context.route.model.Route;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.device.model.DevicesAmount;
import jakarta.persistence.*;
import jakarta.persistence.Entity;
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
@Table(name = "simulation")
public class Simulation {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;
    @Column(name = "name")
    private String name;
    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private Status status = Status.NOT_RUNNING;
    @OneToMany(mappedBy = "simulation", orphanRemoval = true)
    private Set<DevicesAmount> amount = new LinkedHashSet<>();
    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToMany
    @JoinTable(name = "simulation_routes",
            joinColumns = @JoinColumn(name = "simulation_id"),
            inverseJoinColumns = @JoinColumn(name = "routes_id"))
    private Set<Route> routes = new LinkedHashSet<>();

}
