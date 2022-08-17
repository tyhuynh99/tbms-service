package com.shop.tbms.util;

import org.springframework.security.crypto.bcrypt.BCrypt;

public class PasswordUtil {
    public static String encodePassword(String password) {
        return BCrypt.hashpw(password, BCrypt.gensalt());
    }

    public static boolean checkPassword(String password, String encodedPassword) {
        return BCrypt.checkpw(password, encodedPassword);
    }
}
