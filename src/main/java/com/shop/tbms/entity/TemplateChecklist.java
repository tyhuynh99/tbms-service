package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "template_checklist")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class TemplateChecklist extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "content", nullable = false, length = 1000)
    private String content;

    @ManyToOne
    @JoinColumn(name = "template_step_id", nullable = false)
    @ToString.Exclude
    private TemplateStep templateStep;
}
