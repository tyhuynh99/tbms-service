package com.shop.tbms.service.impl;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.cloud.storage.Acl;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.cloud.StorageClient;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.service.FileService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.PostConstruct;
import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import javax.imageio.stream.MemoryCacheImageOutputStream;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.shop.tbms.constant.CommonConstant.IMAGE_CONTENT_TYPE;
import static com.shop.tbms.constant.CommonConstant.PDF_CONTENT_TYPE;
import static com.shop.tbms.constant.CommonConstant.PDF_FOLDER_NAME;
import static com.shop.tbms.constant.MessageConstant.NOT_FORMAT_PDF;

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

        FirebaseOptions options = FirebaseOptions.builder().setCredentials(GoogleCredentials.fromStream(serviceAccount)).setStorageBucket(bucketName).build();
        if (FirebaseApp.getApps().isEmpty()) {
            FirebaseApp.initializeApp(options);
        } else {
            FirebaseApp.getInstance();
        }

        this.bucket = StorageClient.getInstance().bucket();
        log.info("Connect to Firebase Storage successful !");
    }

    @Override
    public FileDTO upload(MultipartFile multipartFile) throws Exception {
        log.info("Start upload file {}", multipartFile.getOriginalFilename());
        // Step 0 validate TODO: validate for video
        validate(multipartFile);
        // Step 1 resize and compress file TODO: compress for video
        MultipartFile compressedFile = compressImage(multipartFile);
        // TODO: Step 2 generate file name
        String filename = generateFilename(compressedFile);

        // Step 3 upload to firebase
        Blob result = bucket.create(filename, compressedFile.getBytes(), compressedFile.getContentType());
        result.createAcl(Acl.of(Acl.User.ofAllUsers(), Acl.Role.READER));
        String viewUrl = String.format(previewUrl, filename);

        log.info("Upload file {} success with url {}", compressedFile.getOriginalFilename(), viewUrl);
        return FileDTO.builder().filename(filename).url(viewUrl).build();
    }

    @Override
    public List<FileDTO> uploadPDF(long orderId, MultipartFile[] multipartFile) throws Exception {
        List<MultipartFile> files = Arrays.asList(multipartFile);
        validatePDF(files);
        List<FileDTO> result = new ArrayList<>();
        files.forEach(x -> {
            try {
                String fileName = x.getOriginalFilename();
                String viewUrl = uploadToServer(x, fileName, orderId);
                FileDTO fileDTO = FileDTO.builder().filename(fileName).url(viewUrl).build();
                result.add(fileDTO);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });

        return result;
    }

    public void validate(MultipartFile multipartFile) throws Exception {
        if (multipartFile.isEmpty()) {
            throw new Exception("File is empty");
        }
        String contentType = multipartFile.getContentType();
        log.info("File contentType {} ", contentType);
        if (contentType == null || !isSupportedContentType(contentType)) {
            throw new Exception("Only support images and video type");
        }
    }

    private boolean isSupportedContentType(String contentType) {
        return IMAGE_CONTENT_TYPE.contains(contentType);
    }

    private MultipartFile compressImage(MultipartFile file) {
        float quality = 0.3f; // Best practice value for compression

        String name = file.getName();
        String imageName = file.getOriginalFilename();
        String contentType = file.getContentType();
        String imageExtension = imageName.substring(imageName.lastIndexOf(".") + 1);

        // Returns an Iterator containing all currently registered ImageWriters that claim to be able to encode the named format.
        // You don't have to register one yourself; some are provided.
        ImageWriter imageWriter = ImageIO.getImageWritersByFormatName(imageExtension).next();
        ImageWriteParam imageWriteParam = imageWriter.getDefaultWriteParam();
        imageWriteParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT); // Check the api value that suites your needs.

        // A compression quality setting of 0.0 is most generically interpreted as "high compression is important,"
        // while a setting of 1.0 is most generically interpreted as "high image quality is important."
        imageWriteParam.setCompressionQuality(quality);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        // MemoryCacheImageOutputStream: An implementation of ImageOutputStream that writes its output to a regular
        // OutputStream, i.e. the ByteArrayOutputStream.
        ImageOutputStream imageOutputStream = new MemoryCacheImageOutputStream(baos);

        // Sets the destination to the given ImageOutputStream or other Object.
        imageWriter.setOutput(imageOutputStream);
        BufferedImage originalImage = null;
        try (InputStream inputStream = file.getInputStream()) {
            originalImage = ImageIO.read(inputStream);
        } catch (IOException e) {
            String info = String.format("compressImage - bufferedImage (file %s)- IOException - message: %s ", imageName, e.getMessage());
            log.error(info);
            return new MockMultipartFile(name, imageName, contentType, baos.toByteArray());
        }
        IIOImage image = new IIOImage(originalImage, null, null);
        try {
            imageWriter.write(null, image, imageWriteParam);
        } catch (IOException e) {
            String info = String.format("compressImage - imageWriter (file %s)- IOException - message: %s ", imageName, e.getMessage());
            log.error(info);
        } finally {
            imageWriter.dispose();
        }

        return new MockMultipartFile(name, imageName, contentType, baos.toByteArray());
    }

    private String generateFilename(MultipartFile file) {
        return file.getOriginalFilename();
    }

    private void validatePDF(List<MultipartFile> files) throws Exception {
        List<String> errFilename = new ArrayList<>();

        files.forEach(x -> {
            if (!PDF_CONTENT_TYPE.contains(x.getContentType())) {
                String filename = x.getOriginalFilename();
                errFilename.add(filename);
            }
        });

        if (!errFilename.isEmpty()) {
            String msg = String.join(", ", errFilename);
            msg = msg.concat(NOT_FORMAT_PDF);
            throw new BusinessException(msg);
        }
    }

    private String uploadToServer(MultipartFile file, String filename, long orderId) throws IOException {
        //Upload to Firebase Storage

        String destination = PDF_FOLDER_NAME.concat(orderId + "/".concat(filename));
        byte[] bytes = file.getBytes();
        String contentType = file.getContentType();

        Blob existedFile = bucket.get(destination);
        int count = 0;
        String tempDestination = "";
        while (existedFile != null) {
            count++;
            tempDestination = destination.concat(String.format("(%d)", count));
            existedFile = bucket.get(tempDestination);
        }

        if (!tempDestination.isEmpty()) {
            destination = tempDestination;
        }

        Blob result = bucket.create(destination, bytes, contentType);
        result.createAcl(Acl.of(Acl.User.ofAllUsers(), Acl.Role.READER));

        return String.format(previewUrl, destination);
    }
}
