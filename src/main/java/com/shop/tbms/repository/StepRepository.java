package com.shop.tbms.repository;

import com.shop.tbms.entity.Step;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface StepRepository extends JpaRepository<Step, Long> {
    Optional<Step> findFirstByCodeAndProcedureOrderId(String code, Long orderId);
}
