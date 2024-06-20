package com.IoTSim.management_server.context.simulation.model;

import com.IoTSim.management_server.context.user.model.User;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.Hibernate;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "simulation_process")
public class SimulationProcess {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id", nullable = false)
    private Long id;
    @Column(name = "simulation_id", nullable = false)
    private Long simulationId;
    @Column(name = "device_id", nullable = false)
    private Long deviceId;
    @Column(name = "supervisor_pid", unique = true)
    private String supervisorPID;


    @OneToMany(mappedBy = "simulationProcess", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<SimulationInstance> simulationInstances = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id")
    private User user;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        SimulationProcess process = (SimulationProcess) o;
        return getId() != null && Objects.equals(getId(), process.getId());
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
