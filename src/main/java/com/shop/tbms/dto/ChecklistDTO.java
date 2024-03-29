package com.shop.tbms.dto;

import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChecklistDTO {
    private Long checklistId;
    private String content;
    private Boolean isChecked;
}
