package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.persistence.*;

@Entity(name = "mold_group_element_progress")
@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldGroupElementProgress extends AbstractAuditingEntity {
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

    @ManyToOne
    @JoinColumn(name = "mold_group_element_id", nullable = false)
    private MoldGroupElement moldGroupElement;
}
