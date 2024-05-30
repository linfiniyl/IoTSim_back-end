package com.IoTSim.management_server.context.attribute.mapper;

import com.IoTSim.management_server.context.attribute.api.AttributeCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeInfoResponse;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateCreateRequest;
import com.IoTSim.management_server.context.attribute.api.AttributeTemplateInfoResponse;
import com.IoTSim.management_server.context.attribute.dto.AttributeDto;
import com.IoTSim.management_server.context.attribute.dto.AttributeTemplateDto;
import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;
import org.mapstruct.Mapper;

@Mapper
public interface AttributeMapper {
    AttributeTemplate AttributeTemplateDtoToAttributeTemplate(AttributeTemplateDto attributeTemplateDto);
    AttributeTemplateDto AttributeTemplateToAttributeTemplateDto(AttributeTemplate attributeTemplate);
    AttributeDto AttributeToAttributeDto(AttributeTemplate attributeTemplate, AttributeAmount attributeAmount);
    AttributeAmount AttributeDtoToAttributeAmount(AttributeDto attributeDto);
    AttributeTemplate AttributeDtoToAttributeTemplate(AttributeDto attributeDto);
    AttributeTemplate AttributeCreateRequestToAttributeTemplate(AttributeCreateRequest attributeCreateRequest);
    AttributeAmount AttributeCreateRequestToAttributeAmount(AttributeCreateRequest attributeCreateRequest);
    AttributeTemplate AttributeTemplateCreateRequestToAttributeTemplate(AttributeTemplateCreateRequest attributeCreateRequest);
    AttributeAmount AttributeTemplateCreateRequestToAttributeAmount(AttributeTemplateCreateRequest attributeCreateRequest);
    AttributeTemplateInfoResponse AttributeTemplateToAttributeTemplateInfoResponse(AttributeTemplate attributeTemplate);
    AttributeInfoResponse AttributeTemplateAndAmountToAttributeInfoResponse(
            AttributeTemplate attributeTemplate, AttributeAmount attributeAmount);

}
