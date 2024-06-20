package com.IoTSim.management_server.context.device.service;

import com.IoTSim.management_server.api.exceptions.DeviceNotFoundException;
import com.IoTSim.management_server.api.exceptions.UserNotFoundException;
import com.IoTSim.management_server.context.device.api.DeviceCreateRequest;
import com.IoTSim.management_server.context.device.api.DeviceInfoResponse;
import com.IoTSim.management_server.context.device.dto.DeviceDto;
import com.IoTSim.management_server.context.device.mapper.DeviceMapper;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.repository.DeviceRepository;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class DeviceService {

    private final DeviceRepository deviceRepository;
    private final UserRepository userRepository;
    private final DeviceMapper mapper;

    @Transactional
    public DeviceDto createDevice(
            DeviceCreateRequest request
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        Device device = Device
                .builder()
                .name(request.getName())
                .description(request.getDescription())
                .picture(request.getPicture())
                .isPrivate(request.getIsPrivate())
                .user(user)
                .build();
        return mapper.deviceToDeviceDto(deviceRepository.saveAndFlush(device));
    }

    @Transactional
    public void updateDevice(
            DeviceDto deviceDto
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        Long ownerId = deviceDto.getUserId();
        if (!userRepository.existById(ownerId)){
            throw new UserNotFoundException();
        }
        if (!deviceRepository.existById(deviceDto.getId())){
            throw new DeviceNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(user.getId(), ownerId)){
            throw new AccessDeniedException("Access Denied");
        }

        deviceRepository.save(
                mapper.deviceDtoToDevice(deviceDto)
        );
    }
    @Transactional
    public void deleteById(
            Long deviceId
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        User owner = deviceRepository
                .findById(deviceId)
                .orElseThrow(DeviceNotFoundException::new)
                .getUser();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(user.getId(), owner.getId())){
            throw new AccessDeniedException("Access Denied");
        }
        deviceRepository.deleteById(deviceId);
    }

    @Transactional
    public DeviceInfoResponse findById(
            Long deviceId
    ) {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }

        Device device = deviceRepository.findById(deviceId).orElseThrow(DeviceNotFoundException::new);
        User owner = device.getUser();
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        if (!Objects.equals(user.getId(), owner.getId())){
            throw new AccessDeniedException("Access Denied");
        }
       return mapper.deviceToDeviceInfoResponse(device);
    }

    public List<DeviceInfoResponse> findAllDevices() {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        user = userRepository.findByEmail(user.getEmail()).orElseThrow(UserNotFoundException::new);
        return mapper.deviceListToDeviceInfoResponseList(
                deviceRepository.findByIsPrivateFalseAndUser(user)
        );
    }

    public Long count() {
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)){
            throw new UserNotFoundException();
        }
        return deviceRepository.countByIsPrivateFalseAndUser(user);
    }
}
