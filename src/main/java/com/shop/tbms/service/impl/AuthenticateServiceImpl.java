package com.shop.tbms.service.impl;

import com.shop.tbms.component.AuthenticateComponent;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.config.exception.ForbiddenException;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.dto.authen.LogoutReqDTO;
import com.shop.tbms.dto.authen.RefreshTokenReqDTO;
import com.shop.tbms.dto.noti.SubscriptionRequestDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.Device;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.mapper.account.AccountToUserDetailsMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.DeviceRepository;
import com.shop.tbms.repository.TemplateStepRepository;
import com.shop.tbms.service.AuthenticateService;
import com.shop.tbms.service.NotificationService;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.JWTUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class AuthenticateServiceImpl implements AuthenticateService {
    @Autowired
    private AuthenticateConstant authenticateConstant;

    @Autowired
    private AccountRepository accountRepository;
    @Autowired
    private TemplateStepRepository templateStepRepository;
    @Autowired
    private DeviceRepository deviceRepository;

    @Autowired
    private AuthenticateComponent authenticateComponent;

    @Autowired
    private AccountToUserDetailsMapper accountToUserDetailsMapper;

    @Autowired
    private NotificationService notificationService;


    @Override
    public LoginResDTO login(LoginReqDTO loginReqDTO) {
        Account account = accountRepository.findFirstByUsername(loginReqDTO.getUsername())
                .orElseThrow(() -> new BusinessException("Invalid username or password"));

        /* check valid account */
        authenticateComponent.checkValidAccount(account, loginReqDTO);

        /* set device token */
        /* delete old device if existed */
        Optional<Device> deviceChk = deviceRepository.findFirstByDeviceId(loginReqDTO.getDeviceToken());
        if (deviceChk.isPresent()) {
            deviceRepository.delete(deviceChk.get());
            deviceRepository.flush();
        }
        /* insert new device token */
        Device device = new Device();
        device.setAccount(account);
        device.setDeviceId(loginReqDTO.getDeviceToken());
        deviceRepository.save(device);

        TbmsUserDetails userDetails = accountToUserDetailsMapper.toUserDetails(account);

        /* subcribe to topic */
        notificationService.subscribeToTopic(
                SubscriptionRequestDTO.builder()
                        .topicName(account.getUsername())
                        .tokens(List.of(loginReqDTO.getDeviceToken())).build()
        );

        String accessToken = authenticateComponent.generateToken(true, userDetails);
        String refreshToken = authenticateComponent.generateToken(false, userDetails);

        return LoginResDTO.builder()
                .token(accessToken)
                .refreshToken(refreshToken)
                .user(userDetails)
                .build();
    }

    @Override
    public LoginResDTO refreshToken(RefreshTokenReqDTO refreshTokenReqDTO) {
        TbmsUserDetails curRefreshTokenUserDetail = JWTUtil.getUserFromToken(refreshTokenReqDTO.getRefreshToken(), authenticateConstant.getKey());

        Account account = accountRepository.findFirstByUsername(curRefreshTokenUserDetail.getUsername())
                .orElseThrow(() -> new BusinessException("Invalid username or password"));

        /* check account is active */
        authenticateComponent.checkActiveAccount(account);

        /* check current device */
        List<Device> listCurrentDevice = account.getDevices();
        log.info("Check refresh account device {}", listCurrentDevice);

        boolean hasDevice = listCurrentDevice.stream()
                .anyMatch(device ->
                        refreshTokenReqDTO.getDeviceToken().equals(device.getDeviceId()));
        if (!hasDevice) {
            log.error("Refresh at new device id {}. Device has not logged in before", refreshTokenReqDTO.getRefreshToken());
            throw new ForbiddenException("Refresh at new device. Device has not logged in before");
        }

        String newAccessToken = authenticateComponent.generateToken(true, curRefreshTokenUserDetail);
        String newRefreshToken = authenticateComponent.generateToken(false, curRefreshTokenUserDetail);

        return LoginResDTO.builder()
                .token(newAccessToken)
                .refreshToken(newRefreshToken)
                .build();
    }

    @Override
    public SuccessRespDTO logout(LogoutReqDTO logoutReqDTO) {
        TbmsUserDetails userDetails = AuthenticationUtil.getUserDetails();

        if (Objects.nonNull(userDetails)) {
            Device device = deviceRepository
                    .findFirstByDeviceIdAndAccountUsername(
                            logoutReqDTO.getDeviceToken(),
                            userDetails.getUsername())
                    .orElseThrow(() ->
                            new BusinessException("Not found device id with username"));

            /* unsubcribe topic */
            notificationService.unsubscribeFromTopic(
                    SubscriptionRequestDTO.builder()
                            .topicName(userDetails.getUsername())
                            .tokens(List.of(device.getDeviceId()))
                            .build());

            deviceRepository.delete(device);

            return SuccessRespDTO.builder()
                    .message(MessageConstant.DELETE_SUCCESS)
                    .build();
        }

        throw new BusinessException("Not found user detail");
    }

    @Override
    public void checkCurrentUserRole(List<Role> listValidRole) {
        TbmsUserDetails userDetails = AuthenticationUtil.getUserDetails();

        Account account = accountRepository.findFirstByUsername(userDetails.getUsername())
                .orElseThrow(() -> new BusinessException("Invalid username or password"));

        // check account is active
        authenticateComponent.checkActiveAccount(account);

        if (!listValidRole.contains(account.getRole())) {
            throw new ForbiddenException("User cannot access to this feature");
        }
    }
}
