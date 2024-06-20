package executions_controller.com.code_generation_module.entities;

import jakarta.persistence.*;
import jakarta.persistence.Entity;
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
    @Column(name = "simulation_id", insertable=false, updatable=false)
    private Long simulationId;
    @Id
    @Column(name = "device_id", insertable=false, updatable=false)
    private Long deviceId;

    @Column(name = "amount_entities")
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
