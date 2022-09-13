package com.shop.tbms.dto.step.report;

import lombok.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@Getter
@Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ReportEvidenceReqDTO {
    private List<MultipartFile> listFile;

    private List<Long> listDeleteEvidenceId;
}
