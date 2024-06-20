package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.Device;
import executions_controller.com.code_generation_module.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DeviceRepository extends JpaRepository<Device, Long> {
    @Query("select (count(d) > 0) from Device d where d.id = ?1")
    boolean existById(long id);


    @Query("select d from Device d where d.isPrivate = false and d.user = ?1")
    List<Device> findByIsPrivateFalseAndUser(User user);

    @Query("select count(d) from Device d where d.isPrivate = false and d.user = ?1")
    long countByIsPrivateFalseAndUser(User user);



}
