package com.IoTSim.management_server.context.attribute.model;


import com.IoTSim.management_server.context.device.model.Device;
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
@Table(name = "attribute_amount")
@IdClass(AttributeAmountId.class)
public class AttributeAmount {
    @Id
    @Column(name = "device_id")
    private Long deviceId;
    @Id
    @Column(name = "attribute_id")
    private Long attributeId;
    @Column(name = "starting_value", nullable = false)
    private Long startingValue;
    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "entity_id", referencedColumnName = "id")
    private Device device;

    @ManyToOne
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(name = "attribute_id", referencedColumnName = "id")
    private AttributeTemplate attributeTemplate;

}
