package com.IoTSim.management_server.context.user.service;

import com.IoTSim.management_server.api.exceptions.UserNotFoundException;

import com.IoTSim.management_server.context.user.api.UserInfoResponse;
import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.mapper.UserMapper;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import com.IoTSim.management_server.context.user.model.Role;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.springframework.util.ObjectUtils;

import java.util.*;

@Service
@RequiredArgsConstructor
public class UserService implements UserDetailsService {

    private final UserRepository userRepository;
    private final UserMapper mapper;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        return userRepository.findByEmail(username)
                .orElseThrow(() -> new UsernameNotFoundException("User not found"));
    }

    @Transactional
    public List<UserInfoResponse> getAllUsers(){
        List<User> userList = userRepository.findAll();
        return mapper.UserListToUserInfoResponseList(userList);
    }

    @Transactional
    public void deleteUserById(Long id){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user) || !userRepository.existById(id))
            throw new UserNotFoundException();
        if (
                Objects.equals(user.getId(), id)
        ) {
            userRepository.deleteById(id);
        } else {
            throw new AccessDeniedException("Access Denied");
        }

    }

    @Transactional
    public void updateUser(UserDto userDto){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user) || !userRepository.existById(userDto.getId()))
            throw new UserNotFoundException();
        if (
                Objects.equals(user.getId(), userDto.getId())
        ) {
            User savingUser = mapper.UserDtoToUser(userDto);
            userRepository.save(savingUser);
        } else {
            throw new AccessDeniedException("Access Denied");
        }

    }

    public long count(){
        return userRepository.count();
    }
    @Bean
    public UserDetailsService userDetailsService(){
        return username -> userRepository.findByEmail(username)
                .orElseThrow(() -> new UsernameNotFoundException("User not found"));
    }

    @Transactional
    public UserInfoResponse getUserById(Long id){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user))
            throw new UserNotFoundException();
        if (
                Objects.equals(user.getId(), id)
        ) {
            return mapper.UserToUserInfoResponse(
                    userRepository.findById(id)
                            .orElseThrow(UserNotFoundException::new)
            );
        } else {
            throw new AccessDeniedException("Access Denied");
        }
    }
}
