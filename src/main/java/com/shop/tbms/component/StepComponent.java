package com.shop.tbms.component;

import com.shop.tbms.constant.MathConstant;
import com.shop.tbms.dto.order.OrderStepRespDTO;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.util.StepUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.List;

@Component
public class StepComponent {
    @Autowired
    private MoldProgressRepository moldProgressRepository;

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
}
