package com.IoTSim.management_server.context.device.model;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(name = "devices_amount")
@IdClass(DevicesAmountId.class)
public class DevicesAmount {

    @Id
    @Column(name = "simulation_id", insertable=false, updatable=false, nullable = false)
    private Long simulationId;
    @Id
    @Column(name = "device_id", insertable=false, updatable=false, nullable = false)
    private Long deviceId;

    @Column(name = "amount_devices", nullable = false)
    private Long amount;

    @MapsId("simulationId")
    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "simulation_id", referencedColumnName = "id")
    private Simulation simulation;
    @MapsId("deviceId")
    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "device_id", referencedColumnName = "id")
    private Device device;

}
