package com.shop.tbms.service.impl;

import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.dto.ProfileDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.NotificationRepository;
import com.shop.tbms.service.UserService;
import com.shop.tbms.util.AuthenticationUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

@Service
@Slf4j
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class UserServiceImpl implements UserService {
    @Autowired
    private AccountRepository accountRepository;
    @Autowired
    private NotificationRepository notificationRepository;

    @Override
    public ProfileDTO getCurrentUserProfile() {
        TbmsUserDetails currentUser = AuthenticationUtil.getUserDetails();
        ProfileDTO profileDTO = null;

        if (Objects.nonNull(currentUser)) {
            Account account = accountRepository.findById(currentUser.getUserId()).orElseThrow();
            if (Objects.nonNull(account.getPosition())) {
                currentUser.setPosition(account.getPosition().getName());
            }

            profileDTO = new ProfileDTO(currentUser);
            profileDTO.setHasUnreadNoti(notificationRepository.existsByReceiverUsernameAndIsRead(currentUser.getUsername(), Boolean.FALSE));
        }
        return profileDTO;
    }
}
