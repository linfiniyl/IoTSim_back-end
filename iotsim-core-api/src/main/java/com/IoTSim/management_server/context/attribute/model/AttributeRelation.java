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
@Table(
        name = "attribute_relation",
        uniqueConstraints = @UniqueConstraint(columnNames = {"attribute_id", "device_id"})
    )
public class AttributeRelation {
    @Id
    @GeneratedValue
    private Long id;

    @Column(name = "attribute_id", insertable=false, updatable=false)
    private Long attributeId;

    @Column(name = "device_id", insertable=false, updatable=false)
    private Long deviceId;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "attribute_id", referencedColumnName = "id")
    private AttributeTemplate attributeTemplate;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "device_id", referencedColumnName = "id")
    private Device device;
}
