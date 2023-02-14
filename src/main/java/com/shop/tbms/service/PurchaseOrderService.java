package com.shop.tbms.service;

import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.PDFDto;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteRespDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface PurchaseOrderService {
    SuccessRespDTO createOrder(OrderCreateReqDTO orderCreateReqDTO);

    SuccessRespDTO updateOrder(OrderUpdateReqDTO orderUpdateReqDTO);

    SuccessRespDTO deleteOrder(Long orderId);

    OrderDetailRespDTO getOrderById(Long orderId);

    Page<OrderListRespDTO> getListOrder(OrderFilterReqDTO filterReqDTO, Pageable pageable);

    List<UpdateExpectedCompleteRespDTO> updateStepExpectedComplete(List<UpdateExpectedCompleteReqDTO> listReqDTO);

    void checkLateOrder();

    void notiNearlyDueOrder();

    List<FileDTO> uploadPDF(Long orderId, MultipartFile[] files) throws Exception;

    ListWrapperDTO<PDFDto> getPDF(Long orderId);

    SuccessRespDTO deletePDF(Long pdfId);
}
