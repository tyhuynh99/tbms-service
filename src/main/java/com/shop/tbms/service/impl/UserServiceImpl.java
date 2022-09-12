package com.shop.tbms.service.impl;

import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.dto.UserAssignedStepDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.AccountAssignStep;
import com.shop.tbms.entity.TemplateStep;
import com.shop.tbms.mapper.StepMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.TemplateStepRepository;
import com.shop.tbms.service.UserService;
import com.shop.tbms.util.AuthenticationUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Slf4j
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class UserServiceImpl implements UserService {
    @Autowired
    private TemplateStepRepository templateStepRepository;
    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private StepMapper stepMapper;

    @Override
    public TbmsUserDetails setUserStepToUser() {
        TbmsUserDetails currentUser = AuthenticationUtil.getUserDetails();

        if (Objects.nonNull(currentUser)) {
            Account account = accountRepository.findById(currentUser.getUserId()).orElseThrow();
            currentUser.setAssignedStep(getStepAssignedUser(account));
        }
        return currentUser;
    }

    private List<UserAssignedStepDTO> getStepAssignedUser(Account account) {
        List<TemplateStep> listStep = templateStepRepository
                .findAllById(
                        account.getAssignedSteps().stream()
                                .map(AccountAssignStep::getStep)
                                .collect(Collectors.toList()));

        return stepMapper.toUserSteps(listStep);
    }
}
