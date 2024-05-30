package executions_controller.com.code_generation_module.Service;


import executions_controller.com.code_generation_module.Entities.AttributeAmount;
import executions_controller.com.code_generation_module.Repository.AttributeAmountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AttributeAmountService {

    @Autowired
    private AttributeAmountRepository attributeAmountRepository;

    public List<AttributeAmount> findAllByEntityId(Long entityId){
        return attributeAmountRepository.findAllByEntityId(entityId);
    }
    public List<AttributeAmount> findAllByAttributeId(Long attributeId){
        return attributeAmountRepository.findAllByAttributeId(attributeId);
    }

    public void createAttributeAmount(AttributeAmount attributeAmount){
        attributeAmountRepository.save(attributeAmount);
    }
    public void deleteById(Long entityId, Long attributeId){
        attributeAmountRepository.deleteByEntityId(entityId, attributeId);
    }
}
