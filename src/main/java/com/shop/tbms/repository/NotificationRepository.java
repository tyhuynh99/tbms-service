package com.shop.tbms.repository;

import com.shop.tbms.entity.TbmsNotification;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface NotificationRepository extends JpaRepository<TbmsNotification, Long>, JpaSpecificationExecutor<TbmsNotification> {
    Page<TbmsNotification> findByReceiverUsername(String receiverUser, Pageable pageable);
    List<TbmsNotification> findByIdInAndReceiverUsername(List<Long> ids, String receiverUsername);
}
