package com.shop.tbms.repository;

import com.shop.tbms.entity.MoldGroup;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MoldGroupRepository extends JpaRepository<MoldGroup, Long> {
}
