package com.shop.tbms.repository;

import com.shop.tbms.entity.ReportLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ReportLogRepository extends JpaRepository<ReportLog, Long> {
    List<ReportLog> findByStepId(Long stepId);
}
