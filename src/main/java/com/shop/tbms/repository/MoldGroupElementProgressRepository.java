package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldGroupElementProgress;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MoldGroupElementProgressRepository extends JpaRepository<MoldGroupElementProgress, Long> {
}
