package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.mold.MoldElementType;
import lombok.*;

import javax.persistence.*;

@Entity(name = "template_mold_element")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TemplateMoldElement extends AbstractAuditingEntity {
    @Id
    @Column(name = "code", nullable = false)
    private String code;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "type", nullable = false)
    private MoldElementType type;

    @Column(name = "is_required", nullable = false)
    private Boolean isRequired;

    @Column(name = "possible_value", length = 1000)
    private String possibleValue;

    @ManyToOne
    @JoinColumn(name = "template_procedure_id", nullable = false)
    @ToString.Exclude
    private TemplateProcedure templateProcedure;
}
