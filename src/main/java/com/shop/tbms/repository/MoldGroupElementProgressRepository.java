package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldGroupElementProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MoldGroupElementProgressRepository extends JpaRepository<MoldGroupElementProgress, Long> {
    List<MoldGroupElementProgress> findAllByStepId(Long stepId);

    long removeByStepIdAndMoldId(Long stepId, Long moldId);

    long deleteByMoldIdIn(List<Long> listMoldId);
}
