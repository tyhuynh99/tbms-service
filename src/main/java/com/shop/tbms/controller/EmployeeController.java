package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.PageResponse;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.account.DeleteEmployeeReqDTO;
import com.shop.tbms.dto.account.EmployeeInListDTO;
import com.shop.tbms.entity.Account_;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.AccountService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping("/employee")
public class EmployeeController {
    @Autowired
    private AccountService accountService;

    @GetMapping("/list")
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY, Role.ACCOUNTANT})
    public ResponseEntity<PageResponse<EmployeeInListDTO>> getListEmployee(
            @SortDefault(sort = {Account_.FULLNAME}, direction = Sort.Direction.ASC)
                    Pageable pageable
    ) {
        return ResponseUtil.buildPageResponse(accountService.getListEmployee(pageable));
    }

    @PostMapping("/delete")
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY, Role.ACCOUNTANT})
    public ResponseEntity<SuccessRespDTO> deleteEmployee(@RequestBody @Valid DeleteEmployeeReqDTO deleteEmployeeReqDTO) {
        return ResponseEntity.ok(accountService.deleteEmployee(deleteEmployeeReqDTO));
    }
}
