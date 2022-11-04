package com.shop.tbms.constant;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

@Component
@Getter
@Setter
@PropertySource(value = "classpath:log-constant.properties", encoding = "UTF-8")
public class LogConstant {
    @Value("${checklist.checked}")
    private String checklistChecked;

    @Value("${checklist.unchecked}")
    private String checklistUnchecked;

    @Value("${progress.checked}")
    private String progressChecked;

    @Value("${progress.unchecked}")
    private String progressUnchecked;
}
