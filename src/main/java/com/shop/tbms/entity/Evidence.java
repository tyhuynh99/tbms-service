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

    @Column(name = "filename", length = 50)
    private String filename;

    @Column(name = "origin_filename")
    private String originFilename;

    @Column(name = "url", length = 1000, nullable = false)
    private String url;

    @Column(name = "is_delete", nullable = false)
    private Boolean isDelete = Boolean.FALSE;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    @ToString.Exclude
    private Step step;

    @ManyToOne
    @JoinColumn(name = "create_at_report_log_id", nullable = false)
    @ToString.Exclude
    private ReportLog createAtReportLog;

    @ManyToOne
    @JoinColumn(name = "delete_at_report_log_id")
    @ToString.Exclude
    private ReportLog deleteAtReportLog;
}
