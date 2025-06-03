package com.IoTSim.management_server.context.device.model;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import jakarta.persistence.*;
import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(
        name = "devices_amount",
        uniqueConstraints = @UniqueConstraint(columnNames = {"simulation_id", "device_id"})
)
public class DevicesAmount {

    @Id
    @GeneratedValue
    private Long id;

    @Column(name = "simulation_id", insertable=false, updatable=false, nullable = false)
    private Long simulationId;

    @Column(name = "device_id", insertable=false, updatable=false, nullable = false)
    private Long deviceId;

    @Column(name = "amount_devices", nullable = false)
    private Long amount;

    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "simulation_id", referencedColumnName = "id")
    private Simulation simulation;

    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "device_id", referencedColumnName = "id")
    private Device device;

}
