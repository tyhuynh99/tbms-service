package com.shop.tbms.dto.mold;

import com.shop.tbms.enumerate.mold.*;
import lombok.*;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

@Getter @Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldGroupDetailResDTO {
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

    private String thinness;
    private String khuonScanCnc;
    private String tiLeRutNuocKhuon;
    private String tiLeRutNuocGo;

    private TaoHoaTheoType taoHoaTheoType;
    private BanLeKhuonType banLeKhuonType;
    private ChuTrenLoiType chuTrenLoiType;
    private VatLieuKhuonType vatLieuKhuonType;

    @NotEmpty
    private List<MoldElementDTO> moldElementList;

    private List<MoldDTO> moldList;
}
