package com.shop.tbms.dto;

import com.shop.tbms.enumerate.mold.MoldPlatingType;
import com.shop.tbms.enumerate.mold.MoldStructure;
import com.shop.tbms.enumerate.mold.MoldType;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldGroupDetailDTO {
    private Long id;
    private MoldType type;
    private MoldStructure structure;
    private MoldPlatingType platingType;
    private boolean hasSon;
    private boolean hasPhuNano;
    private boolean hasBanDien;
    private boolean hasBanLoHoi;
    private Integer numOfPlate;
    private boolean hasNap;
    private boolean hasLoi;
    private boolean hasDe;
    private boolean hasThan;
    private boolean hasTrungKhoan;
    private boolean hasLogo;
    private boolean hasTem;
}
