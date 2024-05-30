package executions_controller.com.code_generation_module.Entities;

import com.IoTSim.management_server.Entities.enums.Status;
import jakarta.persistence.*;
import jakarta.persistence.Entity;
import org.geolatte.geom.Point;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import java.util.List;
import java.util.Set;

@Entity
@Table(name = "simulations")
@IdClass(SimulationId.class)
public class Simulation {
    @Id
    @Column(name = "user_id")
    private Long userId;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", unique = false, nullable = false)
    private Long id;
    @Column(name = "name")
    private String name;
    @ElementCollection
    @CollectionTable(name = "routes",
    joinColumns = {@JoinColumn(name = "simulations_id"), @JoinColumn(name = "user_id")})
    @Column(name = "points")
    private List<Point> routes;
    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private Status status = Status.NOT_RUNNING;
    @OneToMany(mappedBy = "simulation", orphanRemoval = true)
    private Set<EntitiesAmount> amount;
    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "user_id", referencedColumnName = "id", foreignKey = @ForeignKey(name = "FK_SIMULATION_USER"))
    private User user;

    public Simulation(Long userId, Long id, String name, List<Point> routes) {
        this.userId = userId;
        this.id = id;
        this.name = name;
        this.routes = routes;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public void setRoutes(List<Point> routes) {
        this.routes = routes;
    }

    public Long getId() {
        return id;
    }
    public List<Point> getRoutes() {
        return routes;
    }

    public Set<EntitiesAmount> getAmount() {
        return amount;
    }

    public void setAmount(Set<EntitiesAmount> amount) {
        this.amount = amount;
    }

    public Simulation() {
    }
}
