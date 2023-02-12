package com.shop.tbms.service;

import com.shop.tbms.dto.FileDTO;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface FileService {
    FileDTO upload(MultipartFile multipartFile) throws Exception;
    List<FileDTO> uploadPDF(long orderId, MultipartFile[] multipartFile) throws Exception;
}
