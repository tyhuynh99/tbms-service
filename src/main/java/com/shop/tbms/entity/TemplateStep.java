package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.StepType;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "template_step")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TemplateStep extends AbstractAuditingEntity {
    @Id
    @Column(name = "code", nullable = false)
    private String code;

    @Column(name = "type", nullable = false)
    private StepType type;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "sequence_no")
    private Integer sequenceNo;

    @Column(name = "resettable")
    // TODO: check if need this field
    // or every step resettable
    private Boolean resettable;

    @Column(name = "required_evidence", nullable = false)
    private Boolean requiredEvidence;

    @Column(name = "is_start", nullable = false)
    private Boolean isStart;

    @Column(name = "is_end", nullable = false)
    private Boolean isEnd;

    @ManyToOne
    @JoinColumn(name = "template_procedure_id", nullable = false)
    @ToString.Exclude
    private TemplateProcedure templateProcedure;

    @OneToMany(mappedBy = "templateStep")
    @ToString.Exclude
    private List<TemplateChecklist> listTemplateChecklist = new ArrayList<>();

    @OneToMany(mappedBy = "templateStepBefore")
    @ToString.Exclude
    private List<TemplateStepSequence> listTemplateStepBefore = new ArrayList<>();

    @OneToMany(mappedBy = "templateStepAfter")
    @ToString.Exclude
    private List<TemplateStepSequence> listTemplateStepAfter = new ArrayList<>();
}