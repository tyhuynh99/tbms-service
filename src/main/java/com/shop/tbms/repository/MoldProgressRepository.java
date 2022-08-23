package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MoldProgressRepository extends JpaRepository<MoldProgress, Long> {
    List<MoldProgress> findAllByStepId(Long stepId);
}
