package com.shop.tbms.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.dto.ErrorRespDTO;
import com.shop.tbms.enumerate.Role;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;

import static com.shop.tbms.constant.AuthenticateConstant.*;
import static com.shop.tbms.constant.CommonConstant.RES_JSON_TYPE;

@Slf4j
public class JWTUtil {
    public static String parseJwt(HttpServletRequest request) {
        String tokenHeader = request.getHeader(HEADER_AUTHORIZATION);
        log.info("Parse jwt {}", tokenHeader);

        if (!StringUtils.isEmpty(tokenHeader) && tokenHeader.startsWith(TOKEN_PREFIX)) {
            return tokenHeader.replace(TOKEN_PREFIX, StringUtils.EMPTY);
        }

        log.info("Get empty jwt");
        return StringUtils.EMPTY;
    }

    public static TbmsUserDetails getUserFromToken(String token, String key) {
        Claims claims = Jwts.parser().setSigningKey(key.getBytes()).parseClaimsJws(token).getBody();

        return TbmsUserDetails.builder()
                .userId(Long.valueOf((Integer) claims.get(USER_ID)))
                .username((String) claims.get(USERNAME))
                .fullname((String) claims.get(FULLNAME))
                .active((Boolean) claims.get(IS_ACTIVE))
                .role(Role.fromValue((int) claims.get(ROLE)))
                .build();
    }

    public static void setForbiddenResponse(HttpServletResponse response) throws IOException {
        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType(RES_JSON_TYPE);

        ErrorRespDTO errorRespDTO = new ErrorRespDTO();
        errorRespDTO.setErrorCode("Forbidden to request to system");
        errorRespDTO.setErrorMessage("Forbidden to request to system");

        ObjectMapper objectMapper = new ObjectMapper();
        response.getWriter().write(objectMapper.writeValueAsString(errorRespDTO));
    }
}
