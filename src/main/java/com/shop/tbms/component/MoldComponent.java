package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.dto.MoldDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.dto.step.report.ReportMoldProgressReqDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.repository.IssueMoldDetailRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.MoldRepository;
import com.shop.tbms.repository.StepRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
@Slf4j
public class MoldComponent {
    @Autowired
    MoldProgressRepository moldProgressRepository;
    @Autowired
    IssueMoldDetailRepository issueMoldDetailRepository;
    @Autowired
    MoldRepository moldRepository;
    @Autowired
    StepRepository stepRepository;

    public List<Mold> generateMoldEntity(List<String> listMoldReq, PurchaseOrder order) {
        return listMoldReq.stream().map(moldSizeReq -> {
            Mold mold = new Mold();
            mold.setPurchaseOrder(order);
            mold.setSize(moldSizeReq);

            return mold;
        }).collect(Collectors.toList());
    }

    public void updateListMoldInOrder(PurchaseOrder currentOrder, OrderUpdateReqDTO orderUpdateReqDTO) {
        List<Mold> listCurrentMold = currentOrder.getListMold();
        removeDeletedMold(currentOrder.getListMold(), orderUpdateReqDTO.getListSize());
        addNewMold(currentOrder, orderUpdateReqDTO.getListSize());
    }

    private void removeDeletedMold(List<Mold> listCurrentMold, List<String> listUpdatedMold) {
        List<Mold> deletedMold = listCurrentMold.stream()
                .filter(mold -> !listUpdatedMold.contains(mold.getSize()))
                .collect(Collectors.toList());
        List<Long> deletedMoldId = deletedMold.stream().map(Mold::getId).collect(Collectors.toList());

        /* clear relation */
        deletedMold.forEach(mold -> {
            mold.setListMoldProgresses(new ArrayList<>());
            mold.getListMoldProgresses().forEach(moldProgress -> {
                moldProgress.setMold(null);
            });

            mold.setListIssueMold(new ArrayList<>());
            mold.getListIssueMold().forEach(issueMold -> {
                issueMold.setMold(null);
            });
        });
        
        /* delete mold progress */
        moldProgressRepository.deleteByMoldIdIn(deletedMoldId);

        /* delete mold issue */
        issueMoldDetailRepository.deleteByMoldIdIn(deletedMoldId);

        /* delete mold */
        moldRepository.deleteAll(deletedMold);

        /* remove mold */
        listCurrentMold.removeAll(deletedMold);
    }

    private void addNewMold(PurchaseOrder currentOrder, List<String> listUpdatedMold) {
        List<Mold> listCurrentMold = currentOrder.getListMold();
        List<String> listAllCurrentSize = listCurrentMold.stream()
                .map(Mold::getSize)
                .collect(Collectors.toList());
        List<String> newMold = listUpdatedMold.stream()
                .filter(size -> !listAllCurrentSize.contains(size))
                .collect(Collectors.toList());

        /* List all step that has progress entity */
        List<Step> listStepHasProgress = moldProgressRepository
                .findDistinctByStepProcedurePurchaseOrderId(currentOrder.getId())
                .stream()
                .map(MoldProgress::getStep)
                .distinct()
                .collect(Collectors.toList());

        /* Change status of step to IN PROGRESS */
        listStepHasProgress.forEach(step -> step.setStatus(StepStatus.IN_PROGRESS));
        stepRepository.saveAll(listStepHasProgress);

        /* create new Mold entity */
        List<Mold> listNewMold = newMold.stream().map(newSize -> {
            Mold mold = new Mold();
            mold.setSize(newSize);
            mold.setPurchaseOrder(currentOrder);

            List<MoldProgress> listMoldProgress = listStepHasProgress.stream()
                    .map(step -> {
                        MoldProgress moldProgress = new MoldProgress();
                        moldProgress.setMold(mold);
                        moldProgress.setStep(step);
                        moldProgress.setIsCompleted(Boolean.FALSE);

                        return moldProgress;
                    }).collect(Collectors.toList());

            mold.setListMoldProgresses(listMoldProgress);

            return mold;
        }).collect(Collectors.toList());

        moldRepository.saveAll(listNewMold);
    }

    public void validateMoldProgress(List<MoldDTO> listCompletedMoldInPreStep, List<ReportMoldProgressReqDTO> moldProgressReqDTOList) {
        log.info("Start validate mold progress of req = {} and list completed {}", moldProgressReqDTOList, listCompletedMoldInPreStep);

        List<Long> listMoldIdCompleted = listCompletedMoldInPreStep.stream().map(MoldDTO::getId).collect(Collectors.toList());
        List<Long> listMoldIdReqToComplete = moldProgressRepository
                .findAllById(
                        moldProgressReqDTOList.stream()
                                .map(ReportMoldProgressReqDTO::getProgressId)
                                .collect(Collectors.toList()))
                .stream()
                .map(MoldProgress::getId)
                .collect(Collectors.toList());

        listMoldIdReqToComplete.forEach(reqMoldId -> {
            if (!listMoldIdCompleted.contains(reqMoldId)) {
                throw new BusinessException(String.format("Mold ID %s is not complete in previous step", reqMoldId));
            }
        });

        log.info("End validate mold progress without error. Validate pass.");
    }
}
