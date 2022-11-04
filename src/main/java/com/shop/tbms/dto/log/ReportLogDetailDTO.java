package com.shop.tbms.dto.log;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.dto.EvidenceDTO;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@ToString
public class ReportLogDetailDTO {
    private Long id;
    private String description;
    private String reportBy;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime reportAt;
    private String listMold;
    private Boolean isNeedSupport = Boolean.FALSE;
    private List<EvidenceDTO> evidenceCreateList = new ArrayList<>();
    private List<EvidenceDTO> evidenceDeleteList = new ArrayList<>();
}
