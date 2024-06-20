package executions_controller.com.code_generation_module.entities;

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
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(name = "location", columnDefinition = "geometry(Point, 4326)")
    private Point<Position> location;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "route_id", unique = true)
    private Route route;
}
