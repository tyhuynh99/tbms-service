package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldDeliverProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MoldDeliverProgressRepository extends JpaRepository<MoldDeliverProgress, Long> {
    List<MoldDeliverProgress> findAllByStepId(Long stepId);
}
