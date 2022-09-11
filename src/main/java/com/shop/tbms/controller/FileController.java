package com.shop.tbms.controller;

import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.service.FileService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@RestController
@RequestMapping("/file")
public class FileController {
    @Autowired
    private FileService fileService;

    @PostMapping("/upload")
    public ResponseEntity<FileDTO> upload(@RequestParam("file") MultipartFile multipartFile) {
        log.info("HIT -/upload | File Name : {}", multipartFile.getOriginalFilename());
        try {
            return ResponseEntity.ok()
                    .body(fileService.upload(multipartFile));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
