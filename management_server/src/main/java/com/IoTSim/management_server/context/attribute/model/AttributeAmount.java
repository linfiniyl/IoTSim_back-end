package com.IoTSim.management_server.context.attribute.model;


import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import com.IoTSim.management_server.context.user.model.User;
import jakarta.persistence.*;
import jakarta.persistence.Entity;
import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(name = "attribute_amount")
@IdClass(AttributeAmountId.class)
public class AttributeAmount {
    @Id
    @Column(name = "device_id", nullable = false)
    private Long deviceId;
    @Id
    @Column(name = "attribute_id", nullable = false)
    private Long attributeId;
    @Id
    @Column(name = "user_id", nullable = false)
    private Long userId;
    @Id
    @Column(name = "simulation_id", nullable = false)
    private Long simulationId;
    @Column(name = "starting_value", nullable = false)
    private Long startingValue;
    @MapsId("deviceId")
    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "device_id", referencedColumnName = "id")
    private Device device;
    @MapsId("attributeId")
    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "attribute_id", referencedColumnName = "id")
    private AttributeTemplate attributeTemplate;

    @MapsId("user_id")
    @ManyToOne(cascade = {CascadeType.ALL})
    @JoinColumn(name = "user_id",  referencedColumnName = "id")
    private User user;

    @MapsId("simulation_id")
    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "simulation_id", referencedColumnName = "id")
    private Simulation simulation;

}
