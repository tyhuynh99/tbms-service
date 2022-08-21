package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "template_step_sequence")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TemplateStepSequence extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @ManyToOne
    @JoinColumn(name = "template_step_before_id", nullable = false)
    private TemplateStep templateStepBefore;

    @ManyToOne
    @JoinColumn(name = "template_step_after_id", nullable = false)
    private TemplateStep templateStepAfter;
}
