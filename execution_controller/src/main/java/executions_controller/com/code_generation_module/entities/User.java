package executions_controller.com.code_generation_module.entities;

import executions_controller.com.code_generation_module.entities.enums.Role;
import jakarta.persistence.Entity;
import jakarta.persistence.*;
import lombok.*;

import java.util.*;


@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private Long id;
    @Column(name = "first_name")
    private String firstname;
    @Column(name = "last_name")
    private String lastname;
    @Column(name = "password")
    private String password;
    @Transient
    private String passwordConfirm;
    @Column(name = "email", unique = true)
    private String email;
    @Column(name = "is_enabled")
    private Boolean isEnabled;
    @Enumerated
    @Column(name = "role", nullable = false)
    private Role role;

    @OneToMany(mappedBy = "owner", cascade = CascadeType.ALL)
    private Set<AttributeTemplate> attributeTemplates = new LinkedHashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL)
    private Set<Device> entities = new LinkedHashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL)
    private Set<Simulation> simulations = new LinkedHashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<SimulationProcess> simulationProcesses = new LinkedHashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AttributeAmount> attributeAmounts = new LinkedHashSet<>();

}
