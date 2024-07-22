package com.IoTSim.management_server.context.attribute.model;


import com.IoTSim.management_server.context.device.model.Device;
import jakarta.persistence.*;
import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(name = "attribute_relation")
@IdClass(AttributeRelationId.class)
public class AttributeRelation {
    @Id
    @Column(name = "attribute_id")
    private Long attributeId;
    @Id
    @Column(name = "device_id")
    private Long deviceId;

    @MapsId("attribute_id")
    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "attribute_id", referencedColumnName = "id")
    private AttributeTemplate attributeTemplate;

    @MapsId("device_id")
    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "device_id", referencedColumnName = "id")
    private Device device;

}
