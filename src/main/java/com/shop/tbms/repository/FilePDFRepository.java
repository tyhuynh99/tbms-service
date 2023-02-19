package com.shop.tbms.repository;

import com.shop.tbms.entity.FilePDF;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface FilePDFRepository extends JpaRepository<FilePDF, Long> {
    List<FilePDF> findAllByPurchaseOrderId(Long purchaseOrderId);
}
