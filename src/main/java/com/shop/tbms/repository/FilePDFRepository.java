package com.shop.tbms.repository;

import com.shop.tbms.entity.FilePDF;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface FilePDFRepository extends JpaRepository<FilePDF, Long> {
}
