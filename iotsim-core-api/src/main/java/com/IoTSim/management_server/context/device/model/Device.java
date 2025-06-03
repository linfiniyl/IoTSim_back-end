package com.IoTSim.management_server.context.device.model;

import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeRelation;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.util.AccessCheckable;
import jakarta.persistence.*;
import lombok.*;


import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(name = "device")
public class Device implements AccessCheckable {
    @Id
    @GeneratedValue
    private long id;
    @Column(name = "name", nullable = false)
    private String name;
    @Column(name = "description")
    private String description;
    @Column(name = "picture")
    private String picture;
    @Column(name = "is_private", nullable = false)
    private Boolean isPrivate;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @OneToMany(mappedBy = "device", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AttributeRelation> attributeRelations = new LinkedHashSet<>();

    @Override
    public boolean hasAccess(User user) {
        return Objects.equals(user.getId(), this.user.getId()) || !isPrivate;
    }
}
