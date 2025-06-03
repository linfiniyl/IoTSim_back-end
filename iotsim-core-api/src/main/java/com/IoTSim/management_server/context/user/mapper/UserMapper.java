package com.IoTSim.management_server.context.user.mapper;

import com.IoTSim.management_server.context.user.api.UserInfoResponse;
import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.model.User;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface UserMapper {
    User UserDtoToUser(UserDto userDto);
    UserDto UserToUserDto(User user);

    UserInfoResponse UserToUserInfoResponse(User user);
    List<UserInfoResponse> UserListToUserInfoResponseList(List<User> userList);
}
