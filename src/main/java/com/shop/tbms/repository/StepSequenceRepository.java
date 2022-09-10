package com.shop.tbms.repository;

import com.shop.tbms.entity.StepSequence;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StepSequenceRepository extends JpaRepository<StepSequence, Long> {
    List<StepSequence> findByStepBeforeId(Long stepBeforeId);
}
