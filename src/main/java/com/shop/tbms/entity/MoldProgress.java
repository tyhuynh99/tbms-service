package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "mold_progress")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldProgress extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "is_completed")
    private Boolean isCompleted;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    private Step step;

    @ManyToOne
    @JoinColumn(name = "mold_id", nullable = false)
    private Mold mold;
}
