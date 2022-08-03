package com.shop.tbms.config;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.Arrays;
import java.util.Optional;

@Aspect
public class LoggingAspect {

    @Pointcut("within(@org.springframework.stereotype.Repository *)" +
            " || within(@org.springframework.stereotype.Service *)" +
            " || within(@org.springframework.stereotype.Component *)" +
            " || within(@org.springframework.web.bind.annotation.RestController *)")
    public void springBeanPointcut() {
        //empty method because it is just a Pointcut
    }

    @Pointcut("within(com.shop.tbms.repository..*)" +
            " || within(com.shop.tbms.service..*)" +
            " || within(com.shop.tbms.component..*)" +
            " || within(com.shop.tbms.controller..*)")
    public void applicationPackagePointcut() {
        //empty method because it is just a Pointcut
    }

    private Logger logger(JoinPoint joinPoint) {
        return LoggerFactory.getLogger(joinPoint.getSignature().getDeclaringTypeName());
    }

    @AfterThrowing(pointcut = "springBeanPointcut() && applicationPackagePointcut()", throwing = "e")
    public void logAfterThrowing(JoinPoint joinPoint, Throwable e) {
        logger(joinPoint).error("Error in {}() with cause = '{}' and exception = '{}'",
                joinPoint.getSignature().getName(),
                Optional.ofNullable(e.getCause()).map(Throwable::toString).orElse("NULL"),
                e.getMessage());
    }

    @Around("springBeanPointcut() && applicationPackagePointcut()")
    public Object logAround(ProceedingJoinPoint joinPoint) throws Throwable {
        Logger logger = logger(joinPoint);
        if (logger.isDebugEnabled()) {
            logger.debug("Enter: {}() with arguments[s] = {}",
                    joinPoint.getSignature().getName(), Arrays.toString(joinPoint.getArgs()));
        } else {
            logger.info("Enter: {}()", joinPoint.getSignature().getName());
        }
        try {
            Object result = joinPoint.proceed();
            if (logger.isDebugEnabled()) {
                logger.debug("Exit: {}() with result = {}", joinPoint.getSignature().getName(), result);
            } else {
                logger.info("Exit: {}()", joinPoint.getSignature().getName());
            }
            return result;
        } catch (IllegalArgumentException e) {
            logger.error("Illegal Argument: {} in {}()", Arrays.toString(joinPoint.getArgs()), joinPoint.getSignature().getName());
            throw e;
        }
    }

}
