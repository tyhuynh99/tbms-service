package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldGroupElementProgress;
import com.shop.tbms.entity.MoldProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MoldGroupElementProgressRepository extends JpaRepository<MoldGroupElementProgress, Long> {
    List<MoldGroupElementProgress> findAllByStepId(Long stepId);

    long deleteByStepIdAndMoldId(Long stepId, Long moldId);
}
