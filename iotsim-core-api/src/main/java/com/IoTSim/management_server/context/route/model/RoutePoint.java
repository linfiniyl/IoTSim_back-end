package com.IoTSim.management_server.context.route.model;

import jakarta.persistence.*;
import lombok.*;
import org.geolatte.geom.Point;
import org.geolatte.geom.Position;


@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "route_point")
public class RoutePoint {
    @Id
    @GeneratedValue
    private Long id;
    @Column(name = "location", columnDefinition = "geometry(Point,4326)", nullable = false)
    private Point<Position> location;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "route_id", nullable = false)
    private Route route;
}
