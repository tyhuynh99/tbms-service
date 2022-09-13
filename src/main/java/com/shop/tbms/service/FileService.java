package com.shop.tbms.service;

import com.shop.tbms.dto.FileDTO;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

public interface FileService {
    FileDTO upload(MultipartFile multipartFile) throws IOException;
}
