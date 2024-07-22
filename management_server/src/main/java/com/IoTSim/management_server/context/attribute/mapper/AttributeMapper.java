package com.IoTSim.management_server.context.attribute.mapper;

import com.IoTSim.management_server.context.attribute.api.*;
import com.IoTSim.management_server.context.attribute.dto.AttributeDto;
import com.IoTSim.management_server.context.attribute.dto.AttributeTemplateDto;
import com.IoTSim.management_server.context.attribute.model.AttributeAmount;
import com.IoTSim.management_server.context.attribute.model.AttributeRelation;
import com.IoTSim.management_server.context.attribute.model.AttributeTemplate;
import org.apache.tomcat.util.modeler.AttributeInfo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface AttributeMapper {
    AttributeTemplate AttributeTemplateDtoToAttributeTemplate(AttributeTemplateDto attributeTemplateDto);

    AttributeAmount AttributeDtoToAttributeAmount(AttributeDto attributeDto);

    AttributeAmount AttributeCreateRequestToAttributeAmount(AttributeCreateRequest attributeCreateRequest);

    AttributeTemplate AttributeTemplateCreateRequestToAttributeTemplate(AttributeTemplateCreateRequest attributeCreateRequest);

    @Mapping(source = "owner.id", target = "ownerId")
    AttributeTemplateInfoResponse AttributeTemplateToAttributeTemplateInfoResponse(AttributeTemplate attributeTemplate);

    AttributeInfoResponse AttributeAmountToAttributeInfoResponse(AttributeAmount attributeAmount);

    List<AttributeInfoResponse> AttributeAmountListToAttributeInfoResponseList(List<AttributeAmount> attributeAmounts);

    AttributeRelation AttributeRelationCreateRequestToAttributeRelation(
            AttributeRelationCreateRequest attributeRelationCreateRequest
    );

    AttributeRelationInfoResponse AttributeRelationToAttributeRelationInfoResponse(AttributeRelation relation);

    List<AttributeTemplateInfoResponse> AttributeTemplateListToInfoResponseList(List<AttributeTemplate> templates);

    AttributeInfoResponse AttributeAmountAndTemplateToInfoResponse(AttributeTemplate template, AttributeAmount amount);
}
