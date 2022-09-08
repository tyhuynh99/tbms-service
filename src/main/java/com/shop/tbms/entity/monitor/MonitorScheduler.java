package com.shop.tbms.entity.monitor;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.SchedulerFunction;
import lombok.*;

import javax.persistence.*;

@Entity(name = "monitor_scheduler")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MonitorScheduler extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "function_name")
    @Enumerated(EnumType.STRING)
    private SchedulerFunction functionName;
}
