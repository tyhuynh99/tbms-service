package com.shop.tbms.service.impl;

import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.entity.Step;
import com.shop.tbms.mapper.StepMapper;
import com.shop.tbms.repository.StepRepository;
import com.shop.tbms.service.StepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityNotFoundException;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class StepServiceImpl implements StepService {
    @Autowired
    private StepMapper stepMapper;

    @Autowired
    private StepRepository stepRepository;

    @Override
    public StepDTO getStep(Long stepId) {
        Step step = stepRepository.findById(stepId).orElseThrow(EntityNotFoundException::new);
        return stepMapper.toDTO(step);
    }
}
