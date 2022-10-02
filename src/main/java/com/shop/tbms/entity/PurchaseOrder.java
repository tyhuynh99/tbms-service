package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.order.OrderPaymentStatus;
import com.shop.tbms.enumerate.order.OrderStatus;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
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

    @Column(name = "sole_factory_name")
    private String soleFactoryName;

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

    @Column(name = "payment_status")
    private OrderPaymentStatus paymentStatus;

    @Column(name = "delete_by", length = 50)
    private String deleteBy;

    @Column(name = "delete_date")
    private LocalDateTime deleteDate;

    @OneToOne(mappedBy = "purchaseOrder", cascade = CascadeType.ALL)
    @PrimaryKeyJoinColumn
    @ToString.Exclude
    private Procedure procedure;

    @OneToMany(mappedBy = "purchaseOrder", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<MoldElement> listMoldElement = new ArrayList<>();

    @OneToMany(mappedBy = "purchaseOrder", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Mold> listMold = new ArrayList<>();
}
