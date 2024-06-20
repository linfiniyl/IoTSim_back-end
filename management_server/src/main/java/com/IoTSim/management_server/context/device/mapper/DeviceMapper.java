package com.IoTSim.management_server.context.device.mapper;

import com.IoTSim.management_server.context.device.api.DeviceInfoResponse;
import com.IoTSim.management_server.context.device.api.DevicesAmountInfoResponse;
import com.IoTSim.management_server.context.device.dto.DeviceDto;
import com.IoTSim.management_server.context.device.dto.DevicesAmountDto;
import com.IoTSim.management_server.context.device.model.Device;
import com.IoTSim.management_server.context.device.model.DevicesAmount;
import org.mapstruct.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper(componentModel = "spring")
public interface DeviceMapper {
    Device deviceDtoToDevice(DeviceDto deviceDto);
    DeviceDto deviceToDeviceDto(Device device);
    DevicesAmount deviceAmountDtoToDeviceAmount(DevicesAmountDto devicesAmountDto);
    List<Device> deviceDtoListToDeviceList(List<DeviceDto> deviceDtoList);
    List<DeviceDto> deviceListToDeviceDtoList(List<Device> deviceList);
    DevicesAmountDto devicesAmountToDevicesAmountDto(DevicesAmount devicesAmount);
    List<DevicesAmountInfoResponse> devicesAmountListToDevicesAmountInfoResponseList(List<DevicesAmount> devicesAmountList);
    DevicesAmountInfoResponse devicesAmountToDevicesAmountInfoResponse(DevicesAmount devicesAmount);

    DeviceInfoResponse deviceToDeviceInfoResponse(Device device);

    List<DeviceInfoResponse> deviceListToDeviceInfoResponseList(List<Device> deviceList);
}
