package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "issue_mold_detail")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class IssueMoldDetail extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @ManyToOne
    @JoinColumn(name = "issue_id", nullable = false)
    private Issue issue;

    @ManyToOne
    @JoinColumn(name = "mold_id", nullable = false)
    private Mold mold;
}
