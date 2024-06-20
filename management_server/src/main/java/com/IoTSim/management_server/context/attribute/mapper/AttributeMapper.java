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
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface AttributeMapper {
    AttributeTemplate AttributeTemplateDtoToAttributeTemplate(AttributeTemplateDto attributeTemplateDto);
    AttributeAmount AttributeDtoToAttributeAmount(AttributeDto attributeDto);

    AttributeAmount AttributeCreateRequestToAttributeAmount(AttributeCreateRequest attributeCreateRequest);
    AttributeTemplate AttributeTemplateCreateRequestToAttributeTemplate(AttributeTemplateCreateRequest attributeCreateRequest);

    @Mapping(source = "simulationFunctions", target = "simulationFunctions")
    AttributeTemplateInfoResponse AttributeTemplateToAttributeTemplateInfoResponse(AttributeTemplate attributeTemplate);

    AttributeInfoResponse AttributeAmountToAttributeInfoResponse(AttributeAmount attributeAmount);
    List<AttributeInfoResponse> AttributeAmountListToAttributeInfoResponseList(List<AttributeAmount> attributeAmounts);

}
