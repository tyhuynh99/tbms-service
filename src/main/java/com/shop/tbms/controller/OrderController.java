package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.enumerate.Role;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/order")
public class OrderController {
    @PostMapping
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY})
    public ResponseEntity createOrder(@RequestBody @Valid OrderCreateReqDTO orderCreateReqDTO) {
        return null;
    }
}
