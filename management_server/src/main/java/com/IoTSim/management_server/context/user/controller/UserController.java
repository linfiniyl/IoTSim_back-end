package com.IoTSim.management_server.context.user.controller;


import com.IoTSim.management_server.api.Endpoints;
import com.IoTSim.management_server.context.user.dto.UserDto;
import com.IoTSim.management_server.context.user.model.User;
import com.IoTSim.management_server.context.user.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;


@RestController
@RequiredArgsConstructor
@RequestMapping(Endpoints.USERS)
public class UserController {
    private UserService userService;

    @GetMapping
    public ResponseEntity<List<UserDto>> allUsers(){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(userService.getAllUsers());
    }

    @GetMapping(Endpoints.USER_ID)
    public ResponseEntity<UserDto> getUser(@PathVariable Long id){
        return ResponseEntity
                .status(HttpStatus.OK)
                .body(userService.getUserById(id));
    }

    @DeleteMapping(Endpoints.USER_ID)
    public ResponseEntity<?> deleteUser(@RequestBody Long id){
        userService.deleteUserById(id);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }

    @PutMapping(Endpoints.USER_ID)
    public ResponseEntity<?> updateUser(@RequestBody UserDto userDto){
        userService.updateUser(userDto);
        return ResponseEntity
                .status(HttpStatus.OK)
                .build();
    }
}
