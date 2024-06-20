package executions_controller.com.code_generation_module.repository;

import executions_controller.com.code_generation_module.entities.AttributeAmount;
import executions_controller.com.code_generation_module.entities.AttributeAmountId;
import executions_controller.com.code_generation_module.entities.SimulationProcess;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

import java.util.List;


@Repository
public interface AttributeAmountRepository extends JpaRepository<AttributeAmount, AttributeAmountId> {
    @Query(value = "SELECT * FROM attribute_amount " +
            "WHERE device_id = ?1 and simulation_id = ?2, and user_id = ?3" , nativeQuery = true)
    List<AttributeAmount> findAllByDeviceId(
            @NonNull Long deviceId,
            @NonNull Long simulationId,
            @NonNull Long userId
            );

    @Query(value = "SELECT * FROM attribute_amount WHERE attribute_id = ?1" , nativeQuery = true)
    List<AttributeAmount> findAllByAttributeId(@NonNull Long attributeId);

    @Query("select (count(a) > 0) from AttributeAmount a where a.deviceId = ?1 and a.attributeId = ?2")
    boolean existByDeviceIdAndAttributeId(@NonNull Long deviceId, @NonNull Long attributeId);

    @Modifying
    @Query(value = "DELETE FROM attribute_amount WHERE device_id = ?1 and attribute_id = ?2" , nativeQuery = true)
    void deleteByDeviceIdAndAttributeId(@NonNull Long deviceId, @NonNull Long attributeId);

    @Query("""
            select a from AttributeAmount a
            where a.attributeId = ?1 and a.deviceId = ?2 and a.simulationId = ?3 and a.userId = ?4""")
    AttributeAmount findByAttributeIdAndDeviceIdAndSimulationIdAndUserId(
            @NonNull Long attributeId,
            @NonNull Long deviceId,
            @NonNull Long simulationId,
            @NonNull Long userId
    );

    @Query("""
            select a from AttributeAmount a inner join a.user.simulationProcesses simulationProcesses
            where simulationProcesses.id = ?1""")
    List<AttributeAmount> findBySupervisorId(Long id);








}
