package com.IoTSim.management_server.context.route.model;

import jakarta.persistence.*;
import lombok.*;
import org.geolatte.geom.Point;
import org.hibernate.Hibernate;

import java.util.Objects;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Table(name = "route_point")
public class RoutePoint {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(name = "location", columnDefinition = "geometry(Point, 4326)")
    private Point location;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "route_id", unique = true)
    private Route route;
}
