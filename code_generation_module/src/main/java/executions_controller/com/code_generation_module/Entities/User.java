package executions_controller.com.code_generation_module.Entities;
import jakarta.persistence.*;
import jakarta.persistence.Entity;


import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private long id;
    @Column(name = "name")
    private String name;

    @OneToMany(mappedBy = "user", orphanRemoval = true)
    private List<Simulation> simulation;

    public User(String name, long id) {
        this.name = name;
        this.id = id;
    }
    //Добавляет список симуляций в get запрос
    /*
    public void setSimulation(ArrayList<Simulation> simulation) {
        this.simulation = simulation;
    }

    public List<Simulation> getSimulation() {
        return simulation;
    }
*/
    public void setName(String name) {
        this.name = name;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public long getId() {
        return id;
    }

    public User() {
    }
}
