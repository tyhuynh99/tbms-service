package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.step.ReportType;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.enumerate.step.StepType;
import lombok.*;

import javax.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "step")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Step extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "code", nullable = false)
    private String code;

    @Column(name = "type", nullable = false)
    private StepType type;

    @Column(name = "report_type", nullable = false)
    private ReportType reportType;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "sequence_no")
    private Integer sequenceNo;

    @Column(name = "resettable")
    private Boolean resettable;

    @Column(name = "required_evidence", nullable = false)
    private Boolean requiredEvidence;

    @Column(name = "is_start", nullable = false)
    private Boolean isStart;

    @Column(name = "is_end", nullable = false)
    private Boolean isEnd;

    @Column(name = "expected_complete_date")
    private LocalDate expectedCompleteDate;

    @Column(name = "status")
    private StepStatus status;

    @Column(name = "delivered_date")
    private LocalDateTime deliveredDate;

    @Column(name = "received_date")
    private LocalDateTime receivedDate;

    @Column(name = "export_date")
    private LocalDate exportDate;

    @Column(name = "expected_paid_date")
    private LocalDate expectedPaidDate;

    @Column(name = "note", length = 1000)
    private String note;

    @ManyToOne
    @JoinColumn(name = "procedure_id", nullable = false)
    @ToString.Exclude
    private Procedure procedure;

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Checklist> listChecklist = new ArrayList<>();

    @OneToMany(mappedBy = "stepBefore", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<StepSequence> listStepBefore = new ArrayList<>();

    @OneToMany(mappedBy = "stepAfter", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<StepSequence> listStepAfter = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Evidence> listEvidence = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<MoldProgress> listMoldProgresses = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<Issue> listIssue = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<ReportLog> listReportLog = new ArrayList<>();
}
