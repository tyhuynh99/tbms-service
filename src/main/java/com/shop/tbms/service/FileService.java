package com.shop.tbms.service;

import com.shop.tbms.dto.FileDTO;
import org.springframework.web.multipart.MultipartFile;

public interface FileService {
    FileDTO upload(MultipartFile multipartFile) throws Exception;
    FileDTO uploadPDF(MultipartFile[] multipartFile) throws Exception;
}
