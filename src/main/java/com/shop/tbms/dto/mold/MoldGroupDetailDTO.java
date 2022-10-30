package com.shop.tbms.dto.mold;

import com.shop.tbms.enumerate.mold.MoldPlatingType;
import com.shop.tbms.enumerate.mold.MoldStructure;
import com.shop.tbms.enumerate.mold.MoldType;
import lombok.*;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

@Getter @Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldGroupDetailDTO {
    private Long id;
    @NotNull
    private MoldType type;
    @NotNull
    private MoldStructure structure;
    @NotNull
    private MoldPlatingType platingType;
    private boolean hasSon;
    private boolean hasPhuNano;
    private boolean hasBanDien;
    private boolean hasBanLoHoi;
    private int numOfPlate;
    @NotEmpty
    private List<MoldElementDTO> moldElementList;
    @NotEmpty
    private List<String> moldList;
}
