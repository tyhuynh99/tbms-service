package com.shop.tbms.controller;

import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.mold.MoldElementTemplateDTO;
import com.shop.tbms.dto.mold.MoldGroupDetailDTO;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;
import com.shop.tbms.service.MoldService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/mold")
public class MoldController {
    @Autowired
    private MoldService moldService;

    @GetMapping("/element")
    public ResponseEntity<ListWrapperDTO<MoldElementTemplateDTO>> getAllElementTemplate() {
        return ResponseEntity.ok(
                ResponseUtil.wrapListToResp(
                        moldService.getListElementTemplate()
                )
        );
    }

    @GetMapping("/order")
    public ResponseEntity<ListWrapperDTO<MoldGroupDetailDTO>> getListMoldGroupOfOrder(@RequestParam("orderId") Long orderId) {
        return ResponseEntity.ok(ResponseUtil.wrapListToResp(
                moldService.getListElementOfOrder(orderId)
        ));
    }

    @PostMapping("/element/save")
    public ResponseEntity<SuccessRespDTO> save(@RequestBody MoldGroupReqDTO moldGroupReqDTO) {
        return ResponseEntity.ok(moldService.saveMoldGroup(moldGroupReqDTO));
    }

    @DeleteMapping("/group")
    public ResponseEntity<SuccessRespDTO> deleteMoldGroup(@RequestParam("groupId") Long groupId) {
        return ResponseEntity.ok(moldService.deleteMoldGroup(groupId));
    }
}
