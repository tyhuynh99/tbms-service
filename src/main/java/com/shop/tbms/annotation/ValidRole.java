package com.shop.tbms.annotation;

import com.shop.tbms.enumerate.Role;

import java.lang.annotation.*;

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface ValidRole {
    Role[] role();
}
