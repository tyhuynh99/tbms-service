package com.shop.tbms.constant;

import static com.shop.tbms.constant.AppConstant.PASSWORD_MIN_LENGTH;

public class MessageConstant {
    public static final String CREATE_SUCCESS = "Create successfully.";
    public static final String UPDATE_SUCCESS = "Update successfully.";
    public static final String DELETE_SUCCESS = "Delete successfully.";

    public static final String DUPLICATE_USERNAME = "Duplicate username.";
    public static final String PASSWORD_NOT_LONG = "Password must be longer than " + PASSWORD_MIN_LENGTH + " characters.";
    public static final String INVALID_CREATE_ACC_FOR_ROLE = "Cannot create account for this role.";
    public static final String NOT_FOUND_POSITION = "Not found position.";

    public static final String NOT_FORMAT_PDF = " không phải định dạng PDF.";
}
