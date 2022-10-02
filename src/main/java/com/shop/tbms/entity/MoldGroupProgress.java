package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.persistence.*;

@Entity(name = "mold_group_progress")
@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldGroupProgress extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "is_completed")
    private Boolean isCompleted;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    @ToString.Exclude
    private Step step;

    @ManyToOne
    @JoinColumn(name = "mold_group_detail_id", nullable = false)
    @ToString.Exclude
    private MoldGroupDetail moldGroupDetail;
}
