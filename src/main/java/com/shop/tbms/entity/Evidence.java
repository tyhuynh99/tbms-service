package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;

@Entity(name = "evidence")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Evidence extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "description", length = 1000)
    private String description;

    @Column(name = "url", length = 1000, nullable = false)
    private String url;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    private Step step;
}
