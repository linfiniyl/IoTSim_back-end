package com.IoTSim.management_server.context.user.service;

import com.IoTSim.management_server.api.exceptions.UserNotFoundException;
import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.mapper.UserMapper;
import com.IoTSim.management_server.context.user.model.Role;
import com.IoTSim.management_server.models.ConfirmationToken;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.repository.ConfirmationTokenRepository;
import com.IoTSim.management_server.context.user.repository.UserRepository;
import com.IoTSim.management_server.service.EmailService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.*;

@Service
@RequiredArgsConstructor
public class UserService implements UserDetailsService {

    private final UserRepository userRepository;
    private final BCryptPasswordEncoder bCryptPasswordEncoder;
    private final EmailService emailService;
    private final ConfirmationTokenRepository confirmationTokenRepository;
    private final UserMapper mapper;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        Optional<User> user = userRepository.findByUsername(username);

        if (user.isEmpty())
            throw new UsernameNotFoundException("User not found");

        return user.get();
    }

    public List<UserDto> getAllUsers(){
        List<User> userList = userRepository.findAll();
        return mapper.UserListToUserDtoList(userList);
    }

    public void deleteUserById(Long id){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user))
            throw new UserNotFoundException("Current user not found");
        if (userRepository.existById(id))
            throw new UserNotFoundException("User not found");
        if (
                user.getId() == id ||
                        user.getRole().equals(Role.ADMIN)
        ) {
            userRepository.deleteById(id);
        } else {
            throw new AccessDeniedException("Access Denied");
        }

    }
/*
    public ResponseEntity<?> confirmEmail(String confirmationToken) {
        ConfirmationToken token = confirmationTokenRepository.findByConfirmationToken(confirmationToken).get();

        if(token != null)
        {
            User user = userRepository.findByEmailIgnoreCase(token.getUser().getEmail()).get();
            user.setIsEnabled(true);
            userRepository.save(user);
            return ResponseEntity.ok("Email verified successfully!");
        }
        return ResponseEntity.badRequest().body("Error: Couldn't verify email");
    }
*/
    public void updateUser(UserDto userDto){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user))
            throw new UserNotFoundException("Current user not found");
        if (userRepository.existById(userDto.getId()))
            throw new UserNotFoundException("User not found");
        if (
                user.getId() == userDto.getId() ||
                        user.getRole().equals(Role.ADMIN)
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

    public UserDto getUserById(Long id){
        User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (ObjectUtils.isEmpty(user))
            throw new UserNotFoundException("Current user not found");
        if (userRepository.existById(id))
            throw new UserNotFoundException("User not found");
        if (
                user.getId() == id ||
                        user.getRole().equals(Role.ADMIN)
        ) {
            return mapper.UserToUserDto(userRepository.findById(id).get());
        } else {
            throw new AccessDeniedException("Access Denied");
        }
    }
}
