package com.shop.tbms.constant;

import com.shop.tbms.enumerate.mold.MoldType;
import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Getter
@Setter
@Component
@PropertySource("classpath:step-constant.properties")
public class StepConstant {
    public Map<String, String> getListStepNeedUpdateMoldElement() {
        return Map.of(code3D_KHUON, description3D_KHUON);
    }
    public List<String> getListStepNotForFreeFormType() {
        return List.of(code3D_GO, codeCAM_GO, codeCNC_GO_IN, codeGO_TIA_TOT, codeDUC, codeSCAN, codeRAP_KHUON);
    }
    public List<String> getListStepForLoiDe() {
        return List.of(code3D_GO, codeCAM_GO, codeCNC_GO_IN, codeGO_TIA_TOT);
    }

    @Value("${step.code.2D}")
    private String code2D;

    @Value("${step.description.2D}")
    private String description2D;

    @Value("${step.code.DAT_VAT_TU}")
    private String codeDAT_VAT_TU;

    @Value("${step.description.DAT_VAT_TU}")
    private String descriptionDAT_VAT_TU;

    @Value("${step.code.3D_GO}")
    private String code3D_GO;

    @Value("${step.description.3D_GO}")
    private String description3D_GO;


    @Value("${step.code.CAM_GO}")
    private String codeCAM_GO;

    @Value("${step.description.CAM_GO}")
    private String descriptionCAM_GO;


    @Value("${step.code.CNC_GO_IN}")
    private String codeCNC_GO_IN;

    @Value("${step.description.CNC_GO_IN}")
    private String descriptionCNC_GO_IN;


    @Value("${step.code.GO_TIA_TOT}")
    private String codeGO_TIA_TOT;

    @Value("${step.description.GO_TIA_TOT}")
    private String descriptionGO_TIA_TOT;


    @Value("${step.code.DUC}")
    private String codeDUC;

    @Value("${step.description.DUC}")
    private String descriptionDUC;


    @Value("${step.code.SCAN}")
    private String codeSCAN;

    @Value("${step.description.SCAN}")
    private String descriptionSCAN;


    @Value("${step.code.3D_KHUON}")
    private String code3D_KHUON;

    @Value("${step.description.3D_KHUON}")
    private String description3D_KHUON;


    @Value("${step.code.CAM_KHUON}")
    private String codeCAM_KHUON;

    @Value("${step.description.CAM_KHUON}")
    private String descriptionCAM_KHUON;


    @Value("${step.code.CNC_KHUON}")
    private String codeCNC_KHUON;

    @Value("${step.description.CNC_KHUON}")
    private String descriptionCNC_KHUON;


    @Value("${step.code.HOP_KHUON}")
    private String codeHOP_KHUON;

    @Value("${step.description.HOP_KHUON}")
    private String descriptionHOP_KHUON;


    @Value("${step.code.PHONG_DIEN}")
    private String codePHONG_DIEN;

    @Value("${step.description.PHONG_DIEN}")
    private String descriptionPHONG_DIEN;


    @Value("${step.code.RAP_KHUON}")
    private String codeRAP_KHUON;

    @Value("${step.description.RAP_KHUON}")
    private String descriptionRAP_KHUON;


    @Value("${step.code.THU_KHUON}")
    private String codeTHU_KHUON;

    @Value("${step.description.THU_KHUON}")
    private String descriptionTHU_KHUON;

    @Value("${step.code.SUA_KHUON}")
    private String codeSUA_KHUON;

    @Value("${step.description.SUA_KHUON}")
    private String descriptionSUA_KHUON;

    @Value("${step.code.TAO_HOA}")
    private String codeTAO_HOA;

    @Value("${step.description.TAO_HOA}")
    private String descriptionTAO_HOA;

    @Value("${step.code.XI_MA_SON_DANH_BONG}")
    private String codeXI_MA_SON_DANH_BONG;

    @Value("${step.description.XI_MA_SON_DANH_BONG}")
    private String descriptionXI_MA_SON_DANH_BONG;

    @Value("${step.code.KIEM_TRA}")
    private String codeKIEM_TRA;

    @Value("${step.description.KIEM_TRA}")
    private String descriptionKIEM_TRA;

    @Value("${step.code.GIAO_HANG}")
    private String codeGIAO_HANG;

    @Value("${step.description.GIAO_HANG}")
    private String descriptionGIAO_HANG;

    public String getStepToResetToStepWhenChangeMoldDetail() {
        return this.getCodeCAM_GO();
    }

    public String getStepToResetToStepWhenChangeMoldDetail(MoldType curMoldType, MoldType reqMoldType) {
        if ((MoldType.FREEFORM.equals(curMoldType) && !MoldType.FREEFORM.equals(reqMoldType))
                || (!MoldType.FREEFORM.equals(curMoldType) && MoldType.FREEFORM.equals(reqMoldType))
                || MoldType.FREEFORM.equals(curMoldType))
        {
            return getCode3D_GO();
        }
        return this.getStepToResetToStepWhenChangeMoldDetail();
    }

    public String getStepToResetToStepWhenChangeMoldDetail(MoldType curMoldType) {
        if (MoldType.FREEFORM.equals(curMoldType)) {
            return getCode3D_GO();
        }
        return this.getStepToResetToStepWhenChangeMoldDetail();
    }
}
