package com.shop.tbms.service.impl;

import com.shop.tbms.dto.PositionDTO;
import com.shop.tbms.mapper.PositionMapper;
import com.shop.tbms.repository.PositionRepository;
import com.shop.tbms.service.PositionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
@Slf4j
public class PositionServiceImpl implements PositionService {
    @Autowired
    private PositionRepository positionRepository;

    @Autowired
    private PositionMapper positionMapper;


    @Override
    public List<PositionDTO> getListAllPosition() {
        return positionMapper.toListPositionDTO(positionRepository.findAll());
    }
}
