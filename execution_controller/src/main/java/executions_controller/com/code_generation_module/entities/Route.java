package executions_controller.com.code_generation_module.entities;

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
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @Column(name = "route_name")
    private String name;
    @Column(name = "is_private")
    private Boolean isPrivate;

    @OneToMany(mappedBy = "route", orphanRemoval = true)
    private Set<RoutePoint> routePoints = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "owner_id")
    private User owner;

    @OneToMany(mappedBy = "route", cascade = CascadeType.ALL)
    private Set<Simulation> simulations = new LinkedHashSet<>();

}
