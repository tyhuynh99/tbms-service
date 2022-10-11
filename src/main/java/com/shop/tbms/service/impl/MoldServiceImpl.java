package com.shop.tbms.service.impl;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.mold.MoldElementTemplateDTO;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;
import com.shop.tbms.mapper.MoldElementTemplateMapper;
import com.shop.tbms.repository.TemplateMoldElementRepository;
import com.shop.tbms.service.MoldService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
@Slf4j
public class MoldServiceImpl implements MoldService {
    @Autowired
    private TemplateMoldElementRepository templateMoldElementRepository;

    @Autowired
    private MoldElementTemplateMapper moldElementTemplateMapper;

    @Override
    public List<MoldElementTemplateDTO> getListElementTemplate() {
        return moldElementTemplateMapper.toDTOs(templateMoldElementRepository.findAll());
    }

    @Override
    public SuccessRespDTO saveMoldGroup(MoldGroupReqDTO reqDTO) {
        // TODO: validate status to insert or update group

        /**/
        return null;
    }
}
