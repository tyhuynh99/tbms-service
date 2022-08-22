package com.shop.tbms.repository;

import com.shop.tbms.entity.TemplateProcedure;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TemplateProcedureRepository extends JpaRepository<TemplateProcedure, String> {
}
