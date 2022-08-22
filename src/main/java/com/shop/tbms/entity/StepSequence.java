package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "step_sequence")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class StepSequence extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @ManyToOne
    @JoinColumn(name = "step_before_id", nullable = false)
    private Step stepBefore;

    @ManyToOne
    @JoinColumn(name = "step_after_id", nullable = false)
    private Step stepAfter;
}
