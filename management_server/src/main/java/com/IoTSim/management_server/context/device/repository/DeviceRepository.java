package com.IoTSim.management_server.context.device.repository;

import com.IoTSim.management_server.context.device.model.Device;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
@Repository
public interface DeviceRepository extends JpaRepository<Device, Long> {
    boolean existById(Long id);
}
