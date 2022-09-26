package com.shop.tbms.service.impl;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.account.DeleteEmployeeReqDTO;
import com.shop.tbms.dto.account.EmployeeInListDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.mapper.EmployeeMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.service.AccountService;
import com.shop.tbms.specification.EmployeeSpecification;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityNotFoundException;

@Service
@Transactional
@Slf4j
public class AccountServiceImpl implements AccountService {
    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private EmployeeMapper employeeMapper;

    @Autowired
    private MessageConstant messageConstant;

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

        return SuccessRespDTO.builder().message(messageConstant.getDeleteSuccess()).build();
    }
}
