package executions_controller.com.code_generation_module.Service;


import executions_controller.com.code_generation_module.Entities.Entity;
import executions_controller.com.code_generation_module.Repository.EntityRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class EntityService{
    @Autowired
    private EntityRepository entityRepository;

    public void createEntity(Entity entity) {
        entityRepository.save(entity);
    }

    public Entity update(Entity entity, Long entityId) {
        Optional<Entity> optionalEntity = entityRepository.findById(entityId);
        if (optionalEntity.isPresent()){
            Entity existingEntity = optionalEntity.get();
            existingEntity.setName(entity.getName());
            existingEntity.setAttributesId(entity.getAttributesId());
            existingEntity.setDescription(entity.getDescription());
            existingEntity.setPicture(entity.getPicture());
            return entityRepository.save(existingEntity);
        } else {
            return null;
        }
    }

    public void deleteById(Long id) {
        entityRepository.deleteById(id);
    }

    public Optional<Entity> findById(Long id) {
       return entityRepository.findById(id);
    }

    public List<Entity> findAll() {
        return entityRepository.findAll();
    }

    public long count() {
        return entityRepository.count();
    }
}
