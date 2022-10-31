package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.mold.MoldElementTemplateDTO;
import com.shop.tbms.dto.mold.MoldGroupDetailDTO;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;

import java.util.List;

public interface MoldService {
    List<MoldElementTemplateDTO> getListElementTemplate();
    SuccessRespDTO saveMoldGroup(MoldGroupReqDTO reqDTO);
    List<MoldGroupDetailDTO> getListElementOfOrder(long orderId);
    SuccessRespDTO deleteMoldGroup(Long groupId);
}
