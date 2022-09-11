package com.shop.tbms.entity;

import com.shop.tbms.enumerate.ReportActionType;
import lombok.*;

import javax.persistence.*;

@Entity(name = "report_log")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ReportLog {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "action")
    @Enumerated(EnumType.STRING)
    private ReportActionType action;

    @Column(name = "description", columnDefinition = "TEXT")
    private String description;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    @ToString.Exclude
    private Step step;
}
