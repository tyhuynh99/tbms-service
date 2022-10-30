package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.NotificationType;
import lombok.*;

import javax.persistence.*;

@Entity(name = "notification")
@Getter @Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TbmsNotification extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "receiver_username", nullable = false)
    private String receiverUsername;

    @Column(name = "content", length = 5000)
    private String content;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "type", nullable = false)
    @Enumerated(EnumType.STRING)
    private NotificationType type;

    @Column(name = "is_read", nullable = false)
    private Boolean isRead;
}
