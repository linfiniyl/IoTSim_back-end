package com.IoTSim.management_server.context.user.model;


import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.simulation.model.Simulation;
import jakarta.persistence.Entity;
import jakarta.persistence.*;
import lombok.*;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.*;


@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Builder
@Table(name = "user")
public class User implements UserDetails {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private long id;
    @Column(name = "first_name")
    private String firstname;
    @Column(name = "last_name")
    private String lastname;
    @Column(name = "password")
    private String password;
    @Transient
    private String passwordConfirm;
    @Column(name = "email", unique = true)
    private String email;
    @Column(name = "is_enabled")
    private Boolean isEnabled;
    @Enumerated
    private Role role;

    @OneToMany(mappedBy = "owner", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<AttributeTemplate> attributeTemplates = new LinkedHashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<Device> entities = new LinkedHashSet<>();

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<Simulation> simulations = new LinkedHashSet<>();

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return List.of(new SimpleGrantedAuthority(role.name()));
    }

    @Override
    public String getPassword() {
        return password;
    }

    @Override
    public String getUsername() {
        return email;
    }

    @Override
    public boolean isAccountNonExpired() { //Пока true
        return true;
    }

    @Override
    public boolean isAccountNonLocked() { //Пока true
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {  //Пока true
        return true;
    }

    @Override
    public boolean isEnabled() { //Пока true
        return isEnabled;
    }
}
