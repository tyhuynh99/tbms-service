package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "issue")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Issue extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "description", length = 1000, nullable = false)
    private String description;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    private Step step;

    @OneToMany(mappedBy = "issue", cascade = CascadeType.ALL)
    private List<IssueMoldDetail> listIssueMold = new ArrayList<>();
}
