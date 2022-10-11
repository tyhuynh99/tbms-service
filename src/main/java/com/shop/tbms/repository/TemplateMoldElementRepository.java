package com.shop.tbms.repository;

import com.shop.tbms.entity.TemplateMoldElement;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TemplateMoldElementRepository extends JpaRepository<TemplateMoldElement, String> {
}
