package com.shop.tbms.entity;

import com.shop.tbms.entity.common.AbstractAuditingEntity;
import lombok.*;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import java.util.ArrayList;
import java.util.List;

@Entity(name = "position")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Position extends AbstractAuditingEntity {
    @Id
    @Column(name = "code", length = 20)
    private String code;

    @Column(name = "name")
    private String name;

    @OneToMany(mappedBy = "position")
    @ToString.Exclude
    private List<Account> listAccount = new ArrayList<>();
}
