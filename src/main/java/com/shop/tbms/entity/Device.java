package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "device")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Device extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "device_id", nullable = false, unique = true)
    private String deviceId;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    @ToString.Exclude
    private Account account;
}
