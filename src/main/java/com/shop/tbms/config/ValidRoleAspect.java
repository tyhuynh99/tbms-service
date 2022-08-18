package com.shop.tbms.config;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.service.AuthenticateService;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;

import java.lang.reflect.Method;
import java.util.Arrays;

@Aspect
public class ValidRoleAspect {
    @Autowired
    private AuthenticateService authenticateService;

    @Pointcut("within(com.shop.tbms.controller..*)")
    public void validRolePointCut() {
    }

    @Around("validRolePointCut()")
    public Object validRoleAround(ProceedingJoinPoint joinPoint) throws Throwable {
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();

        Method method = methodSignature.getMethod();

        if (method.isAnnotationPresent(ValidRole.class)) {
            ValidRole validRole = method.getAnnotation(ValidRole.class);
            authenticateService.checkCurrentUserRole(Arrays.asList(validRole.role()));
        }

        return joinPoint.proceed();
    }
}
