package com.shop.tbms.repository;

import com.shop.tbms.entity.monitor.MonitorScheduler;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface MonitorSchedulerRepository extends JpaRepository<MonitorScheduler, Long>, JpaSpecificationExecutor<MonitorScheduler> {
}
