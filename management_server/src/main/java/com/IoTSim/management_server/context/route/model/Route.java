package com.IoTSim.management_server.context.route.model;

import jakarta.persistence.*;
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
@Table(name = "route")
public class Route {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(name = "route_name")
    private String name;

    @OneToMany(mappedBy = "route", orphanRemoval = true)
    private Set<RoutePoint> routePoints = new LinkedHashSet<>();

}
