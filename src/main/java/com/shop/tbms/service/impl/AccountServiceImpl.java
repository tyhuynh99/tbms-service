package com.shop.tbms.service.impl;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.account.CreateAccountReqDTO;
import com.shop.tbms.dto.account.DeleteEmployeeReqDTO;
import com.shop.tbms.dto.account.EmployeeInListDTO;
import com.shop.tbms.dto.account.error.CreateAccountErrorDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.mapper.account.EmployeeMapper;
import com.shop.tbms.mapper.account.CreateAccountMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.PositionRepository;
import com.shop.tbms.service.AccountService;
import com.shop.tbms.specification.EmployeeSpecification;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityNotFoundException;

import java.util.Arrays;
import java.util.List;

import static com.shop.tbms.constant.AppConstant.PASSWORD_MIN_LENGTH;

@Service
@Transactional
@Slf4j
public class AccountServiceImpl implements AccountService {
    @Autowired
    private AccountRepository accountRepository;
    @Autowired
    private PositionRepository positionRepository;

    @Autowired
    private EmployeeMapper employeeMapper;
    @Autowired
    private CreateAccountMapper createAccountMapper;

    @Override
    public Page<EmployeeInListDTO> getListEmployee(Pageable pageable) {
        Specification<Account> specification = EmployeeSpecification.genGetListEmployee();
        return accountRepository.findAll(specification, pageable).map(employeeMapper::toListEmployee);
    }

    @Override
    public SuccessRespDTO deleteEmployee(DeleteEmployeeReqDTO deleteEmployeeReqDTO) {
        log.info("Req delete employee {} ", deleteEmployeeReqDTO);
        Account account = accountRepository.findFirstByUsername(deleteEmployeeReqDTO.getUsername())
                .orElseThrow(EntityNotFoundException::new);

        log.info("Start validate account {} to delete", account);

        if (!Role.EMPLOYEE.equals(account.getRole())) {
            throw new BusinessException("User " + deleteEmployeeReqDTO.getUsername() + " is not an employee");
        }

        if (Boolean.FALSE.equals(account.getActive())) {
            throw new BusinessException("User " + deleteEmployeeReqDTO.getUsername() + " has been deleted");
        }

        account.setActive(Boolean.FALSE);
        accountRepository.save(account);

        log.info("Process delete account {}", account);

        return SuccessRespDTO.builder().message(MessageConstant.DELETE_SUCCESS).build();
    }

    @Override
    public SuccessRespDTO createAccount(CreateAccountReqDTO createAccountReqDTO) {
        validateCreateAccount(createAccountReqDTO);

        log.info("Start create account {}", createAccountReqDTO);
        Account newAccount = createAccountMapper.toAccountEntity(createAccountReqDTO);

        accountRepository.save(newAccount);
        log.info("End create account with result {}", newAccount);
        return SuccessRespDTO.builder()
                .message(MessageConstant.CREATE_SUCCESS)
                .build();
    }

    private void validateCreateAccount(CreateAccountReqDTO createAccountReqDTO) {
        log.info("Start validate create account {}", createAccountReqDTO);
        CreateAccountErrorDTO errorDTO = new CreateAccountErrorDTO();
        boolean hasError = false;

        /* validate duplicate account username */
        if (accountRepository.findFirstByUsername(createAccountReqDTO.getUsername()).isPresent()) {
            log.error("Duplicate username {}", createAccountReqDTO.getUsername());
            errorDTO.setUsernameError(MessageConstant.DUPLICATE_USERNAME);
            hasError = true;
        }

        /* validate password length */
        if (StringUtils.isBlank(createAccountReqDTO.getPassword())
                || createAccountReqDTO.getPassword().length() < PASSWORD_MIN_LENGTH) {
            log.error("Password {} is less than {} chars", createAccountReqDTO.getPassword(), PASSWORD_MIN_LENGTH);
            errorDTO.setPasswordError(MessageConstant.PASSWORD_NOT_LONG);
            hasError = true;
        }

        /* validate role */
        if (!Arrays.asList(Role.EMPLOYEE, Role.SECRETARY).contains(createAccountReqDTO.getRole())) {
            log.error("Cannot create account for role {}", createAccountReqDTO.getRole());
            errorDTO.setRoleError(MessageConstant.INVALID_CREATE_ACC_FOR_ROLE);
            hasError = true;
        }

        /* validate position */
        if (Role.EMPLOYEE.equals(createAccountReqDTO.getRole())) {
            if (!positionRepository.existsById(createAccountReqDTO.getPositionCode())) {
                log.error("Not found position code {}", createAccountReqDTO.getPositionCode());
                errorDTO.setPositionError(MessageConstant.NOT_FOUND_POSITION);
                hasError = true;
            }
        } else {
            createAccountReqDTO.setPositionCode(null);
        }

        log.info("End validate req create account {} with hasError={}", createAccountReqDTO, hasError);
        if (hasError) {
            throw new BusinessException("Invalid request to create new account", List.of(errorDTO));
        }
    }
}
