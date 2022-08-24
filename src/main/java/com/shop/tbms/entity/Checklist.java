package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "checklist")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Checklist extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "content", nullable = false, length = 1000)
    private String content;

    @Column(name = "is_checked")
    private Boolean isChecked;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    @ToString.Exclude
    private Step step;
}
