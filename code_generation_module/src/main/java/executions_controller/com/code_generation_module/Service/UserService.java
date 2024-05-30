package executions_controller.com.code_generation_module.Service;

import executions_controller.com.code_generation_module.Entities.User;
import executions_controller.com.code_generation_module.Repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private UserRepository userRepository;

    public List<User> getAllUsers(){
        return userRepository.findAll();
    }

    public Optional<User> getUserById(Long id){
       return userRepository.findById(id);
    }

    public void deleteUserById(Long id){
        userRepository.deleteById(id);
    }

    public User createUser(User user){
        return userRepository.save(user);
    }

    public User updateUserById(Long id,User user){
        Optional<User> optionalUser = userRepository.findById(id);
        if (optionalUser.isPresent()){
            User existingUser = optionalUser.get();
            existingUser.setName(user.getName());
            return userRepository.save(existingUser);
        } else {
            return null;
        }
    }

    public long count(){
        return userRepository.count();
    }
}
