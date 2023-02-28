package com.shop.tbms.service.impl;

import com.shop.tbms.entity.monitor.MonitorScheduler;
import com.shop.tbms.enumerate.SchedulerFunction;
import com.shop.tbms.repository.MonitorSchedulerRepository;
import com.shop.tbms.service.MonitorSchedulerService;
import com.shop.tbms.service.SchedulerExecuteService;
import com.shop.tbms.specification.MonitorSchedulerSpecification;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@Slf4j
public class MonitorSchedulerServiceImpl implements MonitorSchedulerService {
    @Autowired
    private MonitorSchedulerRepository monitorSchedulerRepository;

    @Autowired
    private SchedulerExecuteService schedulerExecuteService;

    @Override
    public void checkLateOrder() {
        log.info("Start check late order");
        schedulerExecuteService.exeCheckLateOrder();
        saveExecuteFunction(SchedulerFunction.CHECK_LATE_ORDER);
    }

    @Override
    public boolean checkExecuteFunction(Specification<MonitorScheduler> specification) {
        return monitorSchedulerRepository.exists(specification);
    }

    private void saveExecuteFunction(SchedulerFunction schedulerFunction) {
        MonitorScheduler monitorScheduler = new MonitorScheduler();
        monitorScheduler.setFunctionName(schedulerFunction);

        monitorSchedulerRepository.save(monitorScheduler);
    }
}
