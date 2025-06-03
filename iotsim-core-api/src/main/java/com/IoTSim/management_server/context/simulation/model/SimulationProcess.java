package com.IoTSim.management_server.context.simulation.model;

import com.IoTSim.management_server.context.device.model.Device;
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
@Table(
        name = "simulation_process",
        uniqueConstraints = @UniqueConstraint(columnNames = {"device_id", "user_id", "simulation_id"})
)
public class SimulationProcess {
    @Id
    @GeneratedValue
    @Column(name = "id", nullable = false)
    private Long id;
    @Column(name = "simulation_id", insertable=false, updatable=false)
    private Long simulationId;
    @Column(name = "device_id", insertable=false, updatable=false)
    private Long deviceId;
    @Column(name = "supervisor_pid", unique = true)
    private String supervisorPID;
    @Column(name = "user_id", insertable=false, updatable=false)
    private Long userId;
    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    private Status simulationStatus;


    @OneToMany(mappedBy = "simulationProcess", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<SimulationInstance> simulationInstances = new LinkedHashSet<>();

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "device_id", nullable = false)
    private Device device;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "simulation_id", nullable = false)
    private Simulation simulation;

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
