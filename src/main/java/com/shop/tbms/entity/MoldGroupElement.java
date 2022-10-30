package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "mold_group_element")
@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldGroupElement extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "checked", nullable = false)
    private Boolean checked;

    @ManyToOne
    @JoinColumn(name = "mold_group_id")
    @ToString.Exclude
    private MoldGroup moldGroup;

    @OneToMany(mappedBy = "moldGroupElement", cascade = CascadeType.ALL)
    @ToString.Exclude
    private List<MoldGroupElementProgress> listMoldGroupElementProgress = new ArrayList<>();
}
