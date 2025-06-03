package com.IoTSim.management_server.context.simulation.model;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.Hibernate;

import java.time.LocalDateTime;
import java.util.Objects;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "simulation_instance")
public class SimulationInstance {

    @Id
    @GeneratedValue
    @Column(name = "id", nullable = false)
    private Long id;
    @Column(name = "device_number", nullable = false)
    private Long deviceNumber;
    @Column(name = "process_starting_time", nullable = false)
    private LocalDateTime processStartingTime;
    @Enumerated(EnumType.STRING)
    @Column(name = "process_status", nullable = false)
    private Status status;
    @Column(name = "simulation_process_id", insertable=false, updatable=false)
    private Long simulationProcessId;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "simulation_process_id")
    private SimulationProcess simulationProcess;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) return false;
        SimulationInstance that = (SimulationInstance) o;
        return getId() != null && Objects.equals(getId(), that.getId());
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
