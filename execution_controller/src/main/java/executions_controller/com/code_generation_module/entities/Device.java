package executions_controller.com.code_generation_module.entities;

import jakarta.persistence.*;
import jakarta.persistence.Entity;
import lombok.*;


import java.util.HashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Entity
@Builder
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
    private Set<DevicesAmount> amount = new HashSet<>();
    @OneToMany(mappedBy = "device" , orphanRemoval = true)
    private Set<AttributeAmount> attributes = new HashSet<>();

    @ManyToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id")
    private User user;

}
