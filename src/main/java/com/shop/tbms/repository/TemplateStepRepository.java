package com.shop.tbms.repository;

import com.shop.tbms.entity.TemplateStep;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TemplateStepRepository extends JpaRepository<TemplateStep, String> {
}
