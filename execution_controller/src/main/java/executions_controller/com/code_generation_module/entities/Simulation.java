package executions_controller.com.code_generation_module.entities;

import executions_controller.com.code_generation_module.entities.enums.Status;
import jakarta.persistence.*;
import lombok.*;

import java.util.LinkedHashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "simulation")
public class Simulation {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;
    @Column(name = "name")
    private String name;
    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    private Status simulationStatus;
    @Column(name = "is_private")
    private Boolean isPrivate = true;
    @OneToMany(mappedBy = "simulation", orphanRemoval = true)
    private Set<DevicesAmount> amount = new LinkedHashSet<>();
    @ManyToOne
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne
    @JoinColumn(name = "route_id")
    private Route route;

    @OneToMany(mappedBy = "simulation", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AttributeAmount> attributeAmounts = new LinkedHashSet<>();

}
