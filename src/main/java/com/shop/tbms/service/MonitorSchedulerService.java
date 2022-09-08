package com.shop.tbms.service;

import com.shop.tbms.entity.monitor.MonitorScheduler;
import org.springframework.data.jpa.domain.Specification;

public interface MonitorSchedulerService {
    void checkLateOrder();

    boolean checkExecuteFunction(Specification<MonitorScheduler> specification);
}
