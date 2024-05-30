package executions_controller.com.code_generation_module.Service;

import executions_controller.com.code_generation_module.Entities.EntitiesAmount;
import executions_controller.com.code_generation_module.Repository.EntitiesAmountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class EntitiesAmountService {

    @Autowired
    private EntitiesAmountRepository entitiesAmountRepository;

    public List<EntitiesAmount> findAllById(Long userId, Long id){
        return entitiesAmountRepository.findAllEntitiesById(userId, id);
    }
    public Optional<EntitiesAmount> findById(Long userId, Long id, Long entityId){
        return entitiesAmountRepository.findEntityById(userId, id, entityId);
    }

    public void createEntitiesAmount(EntitiesAmount entitiesAmount){
        entitiesAmountRepository.save(entitiesAmount);
    }

    public void deleteById(Long userId, Long id, Long entityId){
        entitiesAmountRepository.deleteEntitiesAmounById(userId, id, entityId);
    }

    public EntitiesAmount update(EntitiesAmount entitiesAmount,Long userId, Long id, Long entityId){
        Optional<EntitiesAmount> optionalEntity = entitiesAmountRepository.findEntityById(userId, id, entityId);
        if (optionalEntity.isPresent()){
            EntitiesAmount existingEntity = optionalEntity.get();
            existingEntity.setAmount(entitiesAmount.getAmount());
            return entitiesAmountRepository.save(existingEntity);
        } else {
            return null;
        }
    }
}
