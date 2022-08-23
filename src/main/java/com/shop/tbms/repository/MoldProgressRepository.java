package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MoldProgressRepository extends JpaRepository<MoldProgress, Long> {
}
