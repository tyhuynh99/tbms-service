package com.shop.tbms.service.impl;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.cloud.storage.Acl;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.cloud.StorageClient;
import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.service.FileService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.PostConstruct;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

@Slf4j
@Service
public class FileServiceImpl implements FileService {

    @Value("${firebase.admin-key}")
    private String firebaseKeyFile;

    @Value("${firebase.bucket-name}")
    private String bucketName;

    @Value("${firebase.preview-url}")
    private String previewUrl;

    private Bucket bucket;

    @PostConstruct
    private void initialize() throws Exception {
        FileInputStream serviceAccount;
        try {
            serviceAccount = new FileInputStream(firebaseKeyFile);
        } catch (FileNotFoundException e) {
            log.error("Firebase config file not found");
            throw new Exception(e);
        }

        FirebaseOptions options = FirebaseOptions.builder()
                .setCredentials(GoogleCredentials.fromStream(serviceAccount))
                .setStorageBucket(bucketName)
                .build();
        if (FirebaseApp.getApps().isEmpty()) {
            FirebaseApp.initializeApp(options);
        } else {
            FirebaseApp.getInstance();
        }

        this.bucket = StorageClient.getInstance().bucket();
        log.info("Connect to Firebase Storage successful !");
    }

    @Override
    public FileDTO upload(MultipartFile multipartFile) throws IOException {
        log.info("Start upload file {}", multipartFile.getOriginalFilename());
        // TODO: Step 0 validate

        // TODO: Step 1 resize and compress file

        // TODO: Step 2 generate file name
        String filename = multipartFile.getOriginalFilename();

        // Step 3 upload to firebase
        Blob result = bucket.create(filename, multipartFile.getBytes(), multipartFile.getContentType());
        result.createAcl(Acl.of(Acl.User.ofAllUsers(), Acl.Role.READER));
        String viewUrl = String.format(previewUrl, filename);

        log.info("Upload file {} success with url {}", multipartFile.getOriginalFilename(), viewUrl);
        return FileDTO.builder()
                .filename(filename)
                .url(viewUrl)
                .build();
    }
}
