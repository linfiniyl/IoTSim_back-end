package executions_controller.com.code_generation_module.entities;


import jakarta.persistence.*;
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
    @Column(name = "device_id")
    private Long deviceId;
    @Id
    @Column(name = "attribute_id")
    private Long attributeId;
    @Id
    @Column(name = "simulation_id")
    private Long simulationId;
    @Id
    @Column(name = "user_id")
    private Long userId;
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

    @MapsId("simulationId")
    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    @JoinColumn(name = "simulation_id", nullable = false)
    private Simulation simulation;

    @MapsId("userId")
    @ManyToOne(cascade = CascadeType.ALL, optional = false)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

}
