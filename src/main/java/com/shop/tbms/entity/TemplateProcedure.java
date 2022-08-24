package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "template_procedure")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TemplateProcedure extends AbstractAuditingEntity {
    @Id
    @Column(name = "code", nullable = false)
    private String code;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "priority", nullable = false)
    private Integer priority;

    @OneToMany(mappedBy = "templateProcedure")
    @ToString.Exclude
    private List<TemplateStep> listTemplateStep = new ArrayList<>();

    @OneToMany(mappedBy = "templateProcedure")
    @ToString.Exclude
    private List<TemplateMoldElement> listTemplateMoldElement = new ArrayList<>();
}
