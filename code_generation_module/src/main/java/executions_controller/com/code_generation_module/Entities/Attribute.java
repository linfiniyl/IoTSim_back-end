package executions_controller.com.code_generation_module.Entities;

import com.IoTSim.management_server.Entities.enums.AttributeType;
import com.IoTSim.management_server.Entities.enums.SimulationTypes;
import jakarta.persistence.*;
import jakarta.persistence.Entity;

import java.util.Set;

@Entity
@Table(name = "Attributes")
public class Attribute {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;
    @Column(name = "name")
    private String name;
    @Column(name = "description")
    private String description;
    @Column(name = "value", nullable = false)
    private String value;
    @Column(name = "type")
    private AttributeType type;
    @Column(name = "simulationType")
    private SimulationTypes simulationType;
    @OneToMany(mappedBy = "attribute", orphanRemoval = true)
    private Set<AttributeAmount> entities;



    public Attribute(long id, String name, String description, String value, AttributeType type, SimulationTypes simulationType) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.value = value;
        this.type = type;
        this.simulationType = simulationType;
    }
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public void setId(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public void setSimulationType(SimulationTypes simulationType) {
        this.simulationType = simulationType;
    }

    public SimulationTypes getSimulationType() {
        return simulationType;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setType(AttributeType type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public AttributeType getType() {
        return type;
    }
/*
    public Set<AttributeAmount> getEntities() {
        return entities;
    }

    public void setEntities(Set<AttributeAmount> entities) {
        this.entities = entities;
    }
*/
    public Attribute(){

    }
}
