package com.shop.tbms.constant;

import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@PropertySource("classpath:procedure-constant.properties")
public class ProcedureConstant {
    @Value("${procedure.code.SXKT}")
    private String codeSXKT;

    @Value("${procedure.description.SXKT}")
    private String descriptionSXKT;
}
