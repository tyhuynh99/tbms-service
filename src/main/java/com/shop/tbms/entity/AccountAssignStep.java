package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "account_assign_step")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AccountAssignStep extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "step", length = 50, nullable = false)
    private String step;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;
}
