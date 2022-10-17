package com.shop.tbms.repository;

import com.shop.tbms.entity.Device;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface DeviceRepository extends JpaRepository<Device, Long> {
    Optional<Device> findFirstByDeviceId(String deviceId);
    Optional<Device> findFirstByDeviceIdAndAccountUsername(String deviceId, String username);
}
