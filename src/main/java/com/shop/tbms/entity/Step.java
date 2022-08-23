package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.enumerate.StepType;
import lombok.*;

import javax.persistence.*;
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

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "sequence_no")
    private Integer sequenceNo;

    @Column(name = "resettable")
    // TODO: check if need this field
    // or every step resettable
    private Boolean resettable;

    @Column(name = "required_evidence", nullable = false)
    private Boolean requiredEvidence;

    @Column(name = "is_start", nullable = false)
    private Boolean isStart;

    @Column(name = "is_end", nullable = false)
    private Boolean isEnd;

    @Column(name = "expected_complete_day")
    private Integer expectedCompleteDay;

    @Column(name = "status")
    private StepStatus status;

    @Column(name = "delivered_date")
    private LocalDateTime deliveredDate;

    @Column(name = "received_date")
    private LocalDateTime receivedDate;

    @Column(name = "note", length = 1000)
    private String note;

    @ManyToOne
    @JoinColumn(name = "procedure_id", nullable = false)
    private Procedure procedure;

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    private List<Checklist> listChecklist = new ArrayList<>();

    @OneToMany(mappedBy = "stepBefore", cascade = CascadeType.ALL)
    private List<StepSequence> listStepBefore = new ArrayList<>();

    @OneToMany(mappedBy = "stepAfter", cascade = CascadeType.ALL)
    private List<StepSequence> listStepAfter = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    private List<Evidence> listEvidence = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    private List<MoldProgress> listMoldProgresses = new ArrayList<>();

    @OneToMany(mappedBy = "step", cascade = CascadeType.ALL)
    private List<Issue> listIssue = new ArrayList<>();
}
