package executions_controller.com.code_generation_module.Entities;

import jakarta.persistence.*;


import java.util.ArrayList;
import java.util.Set;

@jakarta.persistence.Entity
@Table(name = "entities")
public class Entity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;
    @Column(name = "name")
    private String name;
    @Column(name = "description")
    private String description;
    @Column(name = "picture")
    private String picture;
    @OneToMany(mappedBy = "entity" , orphanRemoval = true)
    private Set<EntitiesAmount> amount;
    @OneToMany(mappedBy = "entity" , orphanRemoval = true)
    private Set<AttributeAmount> attibutes;

    public void setName(String name) {
        this.name = name;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setPicture(String picture) {
        this.picture = picture;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String getPicture() {
        return picture;
    }

    public void setId(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public Set<EntitiesAmount> getAmount() {
        return amount;
    }

    public void setAmount(Set<EntitiesAmount> amount) {
        this.amount = amount;
    }

    public Set<AttributeAmount> getAttibutes() {
        return attibutes;
    }

    public void setAttibutes(Set<AttributeAmount> attibutes) {
        this.attibutes = attibutes;
    }

    public Entity(long id, String name, String description, String picture) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.picture = picture;
    }

    public Entity() {
    }
}
