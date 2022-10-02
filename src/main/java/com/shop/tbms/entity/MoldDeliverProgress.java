package com.shop.tbms.entity;

import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity(name = "mold_deliver_progress")
@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldDeliverProgress {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "is_completed")
    private Boolean isCompleted;

    @Column(name = "type", nullable = false)
    @Enumerated(value = EnumType.STRING)
    private MoldDeliverProgressType type;

    @Column(name = "action_at")
    private LocalDateTime actionAt;

    @ManyToOne
    @JoinColumn(name = "step_id", nullable = false)
    @ToString.Exclude
    private Step step;

    @ManyToOne
    @JoinColumn(name = "mold_id", nullable = false)
    @ToString.Exclude
    private Mold mold;
}
