package com.shop.tbms.entity;

import lombok.*;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.MappedSuperclass;
import java.time.LocalDateTime;

@MappedSuperclass
@EntityListeners({AuditingEntityListener.class})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
public class AbstractAuditingEntity {
    private static final long serialVersionUID = 1L;

    @CreatedDate
    @Column(
            name = "created_date",
            updatable = false
    )
    protected LocalDateTime createdDate;

    @CreatedBy
    @Column(
            name = "created_by",
            length = 50,
            updatable = false
    )
    protected String createdBy;

    @LastModifiedDate
    @Column(
            name = "updated_date"
    )
    protected LocalDateTime updatedDate;

    @LastModifiedBy
    @Column(
            name = "updated_by",
            length = 50
    )
    protected String updatedBy;
}
