package com.IoTSim.management_server.context.user.repository;

import com.IoTSim.management_server.context.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    Optional<User> findByUsername(String username);
    Optional<User> findByEmailIgnoreCase(String email);
    Boolean existsByUserEmail(String email);

    Optional<User> findByEmail(String email);
    boolean existById(Long id);
}
