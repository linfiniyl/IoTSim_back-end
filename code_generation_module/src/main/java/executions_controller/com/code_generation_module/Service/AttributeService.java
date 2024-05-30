package executions_controller.com.code_generation_module.Service;

import executions_controller.com.code_generation_module.Entities.Attribute;
import executions_controller.com.code_generation_module.Repository.AttributeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class AttributeService {

    @Autowired
    private AttributeRepository attributeRepository;

    public void save(Attribute attribute) {
        attributeRepository.save(attribute);
    }

    public Attribute update(Attribute attribute, Long id) {
        Optional<Attribute> optionalAttribute = attributeRepository.findById(id);
        if (optionalAttribute .isPresent()){
            Attribute existingAttribute = optionalAttribute .get();
            existingAttribute.setName(attribute.getName());
            existingAttribute.setDescription(attribute.getDescription());
            existingAttribute.setType(attribute.getType());
            existingAttribute.setValue(attribute.getValue());
            existingAttribute.setSimulationType(attribute.getSimulationType());

            return attributeRepository.save(existingAttribute);
        } else {
            return null;
        }
    }

    public void deleteById(Long id) {
        attributeRepository.deleteById(id);
    }


    public Optional<Attribute> findById(Long id) {
       return attributeRepository.findById(id);
    }

    public List<Attribute> findAll() {
        return attributeRepository.findAll();
    }

    public long count() {
        return attributeRepository.count();
    }
}
