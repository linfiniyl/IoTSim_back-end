package com.IoTSim.management_server.context.user.mapper;

import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.model.User;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper
public interface UserMapper {
    User UserDtoToUser(UserDto userDto);
    UserDto UserToUserDto(User user);
    List<UserDto> UserListToUserDtoList(List<User> userList);
    List<User> UserDtoListToUserList(List<UserDto> userDtoList);
}
