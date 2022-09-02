package com.shop.tbms.repository;

import com.shop.tbms.entity.IssueMoldDetail;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IssueMoldDetailRepository extends JpaRepository<IssueMoldDetail, Long> {
    long deleteByMoldIdIn(List<Long> listMoldId);
}
