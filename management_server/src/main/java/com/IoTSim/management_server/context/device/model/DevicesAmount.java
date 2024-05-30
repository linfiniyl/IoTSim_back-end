package com.IoTSim.management_server.context.device.model;

import com.IoTSim.management_server.context.simulation.model.Simulation;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "devices_amount")
@IdClass(DevicesAmountId.class)
public class DevicesAmount {

    @Id
    @Column(name = "simulation_id", insertable=false, updatable=false)
    private Long simulationId;
    @Id
    @Column(name = "device_id", insertable=false, updatable=false)
    private Long deviceId;

    @Column(name = "amount_entities")
    private Long amount;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumns({
            @JoinColumn(name = "user_id", referencedColumnName = "user_id"),
            @JoinColumn(name = "simulation_id", referencedColumnName = "id")
    })
    private Simulation simulation;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "device_id", referencedColumnName = "id")
    private Device device;

}
