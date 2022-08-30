package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MathConstant;
import com.shop.tbms.dto.order.OrderStepRespDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.Procedure;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.OrderStatus;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.PurchaseOrderRepository;
import com.shop.tbms.util.StepUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Component
public class StepComponent {
    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    public List<OrderStepRespDTO> setPercentProgress(List<OrderStepRespDTO> listStepDTO, List<Step> listOriginStep) {
        for (OrderStepRespDTO respDTO : listStepDTO) {
            Step originStep = StepUtil.findStepById(respDTO.getId(), listOriginStep);

            if (StepStatus.INIT.equals(originStep.getStatus())) {
                respDTO.setPercentComplete(BigDecimal.ZERO);
            } else {
                respDTO.setPercentComplete(calPercent(originStep));
            }
        }
        return listStepDTO;
    }

    private BigDecimal calPercent(Step step) {
        List<MoldProgress> listMoldProgress = moldProgressRepository.findAllByStepId(step.getId());

        if (CollectionUtils.isEmpty(listMoldProgress)) return BigDecimal.ZERO;

        BigDecimal completedMold = BigDecimal.valueOf(listMoldProgress.stream()
                .filter(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()))
                .count());
        BigDecimal totalMold = BigDecimal.valueOf(listMoldProgress.size());

        return completedMold.divide(totalMold, MathConstant.SCALE, MathConstant.ROUNDING_MODE);
    }

    public void validateUpdateExpectedCompleteData(List<UpdateExpectedCompleteReqDTO> listReqDTO, List<Step> listCurStep) {
        /* Only update step of order status = IN PROGRESS */
        List<PurchaseOrder> listOrder = listCurStep.stream()
                .map(Step::getProcedure)
                .map(Procedure::getPurchaseOrder)
                .distinct()
                .collect(Collectors.toList());

        boolean isInvalidOrderStatus = listOrder.stream()
                .map(PurchaseOrder::getStatus)
                .anyMatch(Predicate.not(OrderStatus.IN_PROGRESS::equals));

        if (isInvalidOrderStatus) {
            throw new BusinessException("Invalid request. Contains completed order.");
        }

        /* if any expected date is in the past -> INVALID */
        boolean isReqDateInvalid = listReqDTO.stream()
                .map(UpdateExpectedCompleteReqDTO::getExpectedCompleteDate)
                .anyMatch(reqDate -> LocalDate.now().isAfter(reqDate));

        if (isReqDateInvalid) {
            throw new BusinessException("Invalid request. Contains date in the past.");
        }
    }
}
