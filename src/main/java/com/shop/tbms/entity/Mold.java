package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "mold")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Mold extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "size", nullable = false)
    private String size;

    @ManyToOne
    @JoinColumn(name = "purchase_order_id", nullable = false)
    private PurchaseOrder purchaseOrder;

    @OneToMany(mappedBy = "mold")
    private List<MoldProgress> listMoldProgress = new ArrayList<>();

    @OneToMany(mappedBy = "mold")
    private List<IssueMoldDetail> listIssueMold = new ArrayList<>();
}
