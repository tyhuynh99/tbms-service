package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "procedure")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Procedure extends AbstractAuditingEntity {
    @Id
    private Long orderId;

    @Column(name = "code", nullable = false)
    private String code;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "priority", nullable = false)
    private Integer priority;

    @OneToOne
    @MapsId
    @JoinColumn(name = "purchase_order_id")
    @ToString.Exclude
    private PurchaseOrder purchaseOrder;

    @OneToMany(mappedBy = "procedure", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Step> listStep = new ArrayList<>();
}
