package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.OrderStatus;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "purchase_order")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class PurchaseOrder extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "code", length = 20, nullable = false)
    private String code;

    @Column(name = "customer_name")
    private String customerName;

    @Column(name = "delivered_date", nullable = false)
    private LocalDate deliveredDate;

    @Column(name = "is_urgent")
    private Boolean isUrgent;

    @Column(name = "is_late")
    private Boolean isLate;

    @Column(name = "is_deleted")
    private Boolean isDeleted;

    @Column(name = "status")
    private OrderStatus status;

    @OneToOne(mappedBy = "purchaseOrder", cascade = CascadeType.ALL)
    @PrimaryKeyJoinColumn
    private Procedure procedure;

    @OneToMany(mappedBy = "purchaseOrder")
    private List<MoldElement> listMoldElement = new ArrayList<>();

    @OneToMany(mappedBy = "purchaseOrder")
    private List<Mold> listMold = new ArrayList<>();
}
