package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldDeliverProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MoldDeliverProgressRepository extends JpaRepository<MoldDeliverProgress, Long> {
}
