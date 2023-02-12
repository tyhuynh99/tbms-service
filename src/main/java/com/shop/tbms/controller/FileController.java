package com.shop.tbms.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.service.FileService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

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

    @PostMapping(value = "/multipart", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity testMultipart(@RequestPart("fileDTO") String fileDTOs, @RequestPart("files") MultipartFile file) {
        ObjectMapper objectMapper = new ObjectMapper();
        FileDTO fileDTO = null;
        try {
            fileDTO = objectMapper.readValue(fileDTOs, FileDTO.class);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
        return ResponseEntity.ok(file.getOriginalFilename() + fileDTO.toString());
    }

    @PostMapping(value = "/pdf", consumes = {MediaType.APPLICATION_PDF_VALUE, "multipart/form-data"})
    public ResponseEntity<List<FileDTO>> uploadPDF(@RequestParam("orderId") long orderId, @RequestPart("files") MultipartFile[] files) throws Exception {
        List<FileDTO> result = fileService.uploadPDF(orderId, files);
        return ResponseEntity.ok(result);
    }
}
