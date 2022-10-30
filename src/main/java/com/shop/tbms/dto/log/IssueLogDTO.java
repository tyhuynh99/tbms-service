package com.shop.tbms.dto.log;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;

@Getter
@Setter
@ToString
public class IssueLogDTO {
    private String reportBy;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime reportAt;
    private String listMold;
    private Boolean isNeedSupport = Boolean.FALSE;
    private String description;
}
