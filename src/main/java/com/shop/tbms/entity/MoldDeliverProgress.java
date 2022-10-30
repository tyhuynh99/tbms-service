package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity(name = "mold_deliver_progress")
@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldDeliverProgress extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "is_completed")
    private Boolean isCompleted;

    @Column(name = "type", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private MoldDeliverProgressType type;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    private Step step;

    @ManyToOne
    @JoinColumn(name = "mold_id", nullable = false)
    private Mold mold;
}
