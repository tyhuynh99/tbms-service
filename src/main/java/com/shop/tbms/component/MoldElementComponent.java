package com.shop.tbms.component;

import com.shop.tbms.entity.MoldElement;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.TemplateMoldElement;
import com.shop.tbms.mapper.MoldElementMapper;
import com.shop.tbms.repository.MoldElementRepository;
import com.shop.tbms.repository.TemplateMoldElementRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MoldElementComponent {
    @Autowired
    private MoldElementRepository moldElementRepository;
    @Autowired
    private TemplateMoldElementRepository templateMoldElementRepository;

    @Autowired
    private MoldElementMapper moldElementMapper;

    public void createMoldElement(PurchaseOrder currentOrder) {
        List<TemplateMoldElement> listTemplate = templateMoldElementRepository.findAll();
        List<MoldElement> listMoldElement = moldElementMapper.fromTemplates(listTemplate);
        listMoldElement.forEach(element -> element.setPurchaseOrder(currentOrder));
        moldElementRepository.saveAll(listMoldElement);
    }
}
