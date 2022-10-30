package com.shop.tbms.dto.log;

import com.shop.tbms.dto.EvidenceDTO;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@ToString
public class ReportLogDetailDTO {
    private Long id;
    private String description;
    private List<EvidenceDTO> evidenceCreateList = new ArrayList<>();
    private List<EvidenceDTO> evidenceDeleteList = new ArrayList<>();
}
