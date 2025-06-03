package com.IoTSim.management_server.context.user.service;

import com.IoTSim.management_server.api.exceptions.UserNotFoundException;

import com.IoTSim.management_server.context.user.api.UserInfoResponse;
import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.mapper.UserMapper;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import org.springframework.util.ObjectUtils;

import java.util.*;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserService implements UserDetailsService {


    private final UserRepository userRepository;
    private final UserMapper mapper;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        log.debug("Loading user by username: {}", username);
        UserDetails userDetails = userRepository.findByEmail(username)
                .orElseThrow(() -> {
                    log.error("User with username {} not found", username);
                    return new UsernameNotFoundException("User not found");
                });
        log.info("User with username {} loaded successfully", username);
        return userDetails;
    }

    @Transactional(readOnly = true)
    public List<UserInfoResponse> getAllUsers() {
        log.debug("Fetching all users from database");
        List<User> userList = userRepository.findAll();
        log.info("Retrieved all users, count: {}", userList.size());
        return mapper.UserListToUserInfoResponseList(userList);
    }


    @Transactional
    public void deleteUserById(Long id) {
        log.debug("Attempting to delete user with ID: {}", id);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user) || !userRepository.existById(id)) {
            log.error("User not found or does not exist with ID: {}", id);
            throw new UserNotFoundException();
        }

        if (Objects.equals(user.getId(), id)) {
            userRepository.deleteById(id);
            log.info("User with ID {} deleted successfully", id);
        } else {
            log.error("Access denied for user with ID {}", id);
            throw new AccessDeniedException("Access Denied");
        }
    }

    @Transactional
    public void updateUser(UserDto userDto) {
        log.debug("Updating user with ID: {}", userDto.getId());
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user) || !userRepository.existById(userDto.getId())) {
            log.error("User not found with ID: {}", userDto.getId());
            throw new UserNotFoundException();
        }

        if (Objects.equals(user.getId(), userDto.getId())) {
            User savingUser = mapper.UserDtoToUser(userDto);
            userRepository.save(savingUser);
            log.info("User with ID {} updated successfully", userDto.getId());
        } else {
            log.error("Access denied for user with ID {}", userDto.getId());
            throw new AccessDeniedException("Access Denied");
        }
    }

    @Bean
    public UserDetailsService userDetailsService() {
        return username -> {
            log.debug("Fetching user details for username: {}", username);
            UserDetails userDetails = userRepository.findByEmail(username)
                    .orElseThrow(() -> {
                        log.error("User with username {} not found", username);
                        return new UsernameNotFoundException("User not found");
                    });
            log.info("User with username {} loaded successfully", username);
            return userDetails;
        };
    }

    @Transactional
    public UserInfoResponse getUserById(Long id) {
        log.debug("Fetching user by ID: {}", id);
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();

        if (ObjectUtils.isEmpty(user)) {
            log.error("Authenticated user not found in security context");
            throw new UserNotFoundException();
        }

        if (Objects.equals(user.getId(), id)) {
            UserInfoResponse response = mapper.UserToUserInfoResponse(
                    userRepository.findById(id)
                            .orElseThrow(() -> {
                                log.error("User not found with ID: {}", id);
                                return new UserNotFoundException();
                            })
            );
            log.info("User with ID {} retrieved successfully", id);
            return response;
        } else {
            log.error("Access denied for user with ID {}", id);
            throw new AccessDeniedException("Access Denied");
        }
    }
}
