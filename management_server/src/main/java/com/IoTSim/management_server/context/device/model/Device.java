package com.IoTSim.management_server.context.device.model;

import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.user.model.User;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@jakarta.persistence.Entity
@Table(name = "device")
public class Device {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;
    @Column(name = "name")
    private String name;
    @Column(name = "description")
    private String description;
    @Column(name = "picture")
    private String picture;
    @Column(name = "is_private")
    private Boolean isPrivate;


    @OneToMany(mappedBy = "device" , orphanRemoval = true)
    private Set<DevicesAmount> amount;
    @OneToMany(mappedBy = "device" , orphanRemoval = true)
    private Set<AttributeAmount> attributes;

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id")
    private User user;

}
