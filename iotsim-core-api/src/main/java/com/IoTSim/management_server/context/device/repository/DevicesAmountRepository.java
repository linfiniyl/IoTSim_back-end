package com.IoTSim.management_server.context.device.repository;

import com.IoTSim.management_server.context.device.model.DevicesAmount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface DevicesAmountRepository extends JpaRepository<DevicesAmount, Long> {

    @Query(value = "SELECT * FROM devices_amount WHERE simulation_id = ?1" , nativeQuery = true)
    List<DevicesAmount> findAllDevicesById(Long simulationId);

    @Modifying
    @Query(value = "DELETE FROM devices_amount WHERE simulation_id = ?1 and device_id = ?2", nativeQuery = true)
    void deleteDevicesAmountById(Long simulationId, Long deviceId);

    @Query(value = "SELECT * FROM devices_amount " +
            "WHERE simulation_id = ?1 and device_id = ?2" , nativeQuery = true)
    Optional<DevicesAmount> findAmountById(Long simulationId, Long deviceId);

    @Query(value = "SELECT sum(amount_devices) FROM devices_amount WHERE simulation_id = ?1", nativeQuery = true)
    Long sumDevicesBySimulationId(Long simulationId);

    @Query("select (count(d) > 0) from DevicesAmount d where d.simulationId = ?1 and d.deviceId = ?2")
    boolean existsBySimulationIdAndDeviceId(Long simulationId, Long deviceId);

    @Query("""
            select count(da) from DevicesAmount da
            left join AttributeAmount am
            on am.simulationId = da.simulationId
            left join AttributeRelation aa
            on aa.deviceId = da.deviceId
            where da.simulationId = ?1
            """)
    long sumDevicesWithoutAttributesBySimulationId(Long simulationId);

}
