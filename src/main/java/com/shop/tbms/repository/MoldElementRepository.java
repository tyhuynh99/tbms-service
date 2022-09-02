package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldElement;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MoldElementRepository extends JpaRepository<MoldElement, Long> {
}
