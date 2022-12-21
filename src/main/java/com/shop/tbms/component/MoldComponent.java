package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.dto.step.report.ReportProgressReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import com.shop.tbms.enumerate.step.ReportType;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.repository.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
@Slf4j
public class MoldComponent {
    @Autowired
    MoldProgressRepository moldProgressRepository;
    @Autowired
    MoldDeliverProgressRepository moldDeliverProgressRepository;
    @Autowired
    MoldGroupElementProgressRepository moldGroupElementProgressRepository;
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
        removeDeletedMold(currentOrder.getListMold(), orderUpdateReqDTO.getListMoldSize());
        addNewMold(currentOrder, orderUpdateReqDTO.getListMoldSize());
    }

    private void removeDeletedMold(List<Mold> listCurrentMold, List<MoldDTO> listReqMold) {
        List<Long> listReqMoldId = listReqMold.stream()
                .filter(moldDTO -> Objects.nonNull(moldDTO.getId()))
                .map(MoldDTO::getId)
                .collect(Collectors.toList());

        List<Mold> deletedMold = listCurrentMold.stream()
                .filter(mold -> !listReqMoldId.contains(mold.getId()))
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

        /* delete mold deliver */
        moldDeliverProgressRepository.deleteByMoldIdIn(deletedMoldId);

        /* delete mold element progress */
        moldGroupElementProgressRepository.deleteByMoldIdIn(deletedMoldId);

        /* delete mold issue */
        issueMoldDetailRepository.deleteByMoldIdIn(deletedMoldId);

        /* delete mold */
        moldRepository.deleteAll(deletedMold);

        /* remove mold */
        listCurrentMold.removeAll(deletedMold);
    }

    private void addNewMold(PurchaseOrder currentOrder, List<MoldDTO> newMold) {
        List<String> listNewSize = newMold.stream()
                .filter(moldDTO -> Objects.isNull(moldDTO.getId()))
                .map(MoldDTO::getSize)
                .collect(Collectors.toList());

        /* create new Mold entity */
        List<Mold> listNewMold = listNewSize.stream().map(newSize -> {
            Mold mold = new Mold();
            mold.setSize(newSize);
            mold.setPurchaseOrder(currentOrder);

            List<MoldProgress> listMoldProgress = currentOrder.getProcedure().getListStep().stream()
                    .filter(step -> ReportType.BY_MOLD.equals(step.getReportType()))
                    .map(step -> {
                        MoldProgress moldProgress = new MoldProgress();
                        moldProgress.setMold(mold);
                        moldProgress.setStep(step);
                        moldProgress.setIsCompleted(Boolean.FALSE);

                        return moldProgress;
                    }).collect(Collectors.toList());

            List<MoldDeliverProgress> listMoldDeliverProgress = currentOrder.getProcedure().getListStep().stream()
                    .filter(step -> ReportType.BY_MOLD_SEND_RECEIVE.equals(step.getReportType()))
                    .map(step -> {
                        MoldDeliverProgress sendProgress = new MoldDeliverProgress();
                        sendProgress.setMold(mold);
                        sendProgress.setStep(step);
                        sendProgress.setIsCompleted(Boolean.FALSE);
                        sendProgress.setType(MoldDeliverProgressType.SEND);

                        MoldDeliverProgress receiveProgress = new MoldDeliverProgress();
                        receiveProgress.setMold(mold);
                        receiveProgress.setStep(step);
                        receiveProgress.setIsCompleted(Boolean.FALSE);
                        receiveProgress.setType(MoldDeliverProgressType.RECEIVE);

                        return List.of(sendProgress, receiveProgress);
                    }).flatMap(Collection::stream)
                    .collect(Collectors.toList());

            /* Skip mold group element progress, will generate at step add to mold group */
            mold.setListMoldProgresses(listMoldProgress);
            mold.setListMoldDeliverProgresses(listMoldDeliverProgress);

            return mold;
        }).collect(Collectors.toList());

        moldRepository.saveAll(listNewMold);
    }
}
