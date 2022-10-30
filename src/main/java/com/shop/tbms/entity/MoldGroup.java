package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.mold.MoldPlatingType;
import com.shop.tbms.enumerate.mold.MoldStructure;
import com.shop.tbms.enumerate.mold.MoldType;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "mold_group")
@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldGroup extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "type", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private MoldType type;

    @Column(name = "structure", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private MoldStructure structure;

    @Column(name = "plating_type", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private MoldPlatingType platingType;

    @Column(name = "has_son", nullable = false)
    private Boolean hasSon;

    @Column(name = "has_phu_nano", nullable = false)
    private Boolean hasPhuNano;

    @Column(name = "has_ban_dien", nullable = false)
    private Boolean hasBanDien;

    @Column(name = "has_eva")
    private Boolean hasBanLoHoi;

    @Column(name = "num_of_plate", nullable = false)
    private Integer numOfPlate;

    @OneToMany(mappedBy = "moldGroup", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<MoldGroupElement> listMoldGroupElement = new ArrayList<>();

    @OneToMany(mappedBy = "moldGroup", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Mold> listMold = new ArrayList<>();

    @ManyToOne
    @JoinColumn(name = "purchase_order_id", nullable = false)
    @ToString.Exclude
    private PurchaseOrder purchaseOrder;


}
