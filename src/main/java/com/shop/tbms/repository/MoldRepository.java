package com.shop.tbms.repository;

import com.shop.tbms.entity.Mold;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MoldRepository extends JpaRepository<Mold, Long> {
}
