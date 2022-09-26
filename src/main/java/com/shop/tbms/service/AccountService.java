package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.account.DeleteEmployeeReqDTO;
import com.shop.tbms.dto.account.EmployeeInListDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface AccountService {
    Page<EmployeeInListDTO> getListEmployee(Pageable pageable);
    SuccessRespDTO deleteEmployee(DeleteEmployeeReqDTO deleteEmployeeReqDTO);
}
