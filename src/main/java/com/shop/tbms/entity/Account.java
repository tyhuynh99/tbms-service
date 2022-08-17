package com.shop.tbms.entity;

import com.shop.tbms.enumerate.Role;
import lombok.*;

import javax.persistence.*;
import java.util.List;

@Entity(name = "account")
@Setter
@Getter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class Account extends AbstractAuditingEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "username", length = 50, unique = true)
    private String username;

    @Column(name = "password")
    private String password;

    @Column(name = "fullname")
    private String fullname;

    @Column(name = "role")
    private Role role;

    @Column(name = "active")
    private Boolean active;

    @OneToMany(mappedBy = "account")
    private List<Device> devices;

    @OneToMany(mappedBy = "account")
    private List<AccountAssignStep> assignedSteps;
}
