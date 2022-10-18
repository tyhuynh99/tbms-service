package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "report_log")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ReportLog extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "description", columnDefinition = "TEXT")
    private String description;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    @ToString.Exclude
    private Step step;

    @OneToMany(mappedBy = "reportLog", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Evidence> evidenceList = new ArrayList<>();
}
