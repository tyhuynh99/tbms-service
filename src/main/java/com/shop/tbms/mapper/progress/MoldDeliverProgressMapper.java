package com.shop.tbms.mapper.progress;

import com.shop.tbms.dto.step.detail.progress.MoldDeliverProgressDTO;
import com.shop.tbms.entity.MoldDeliverProgress;
import com.shop.tbms.entity.MoldProgress;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

@Mapper(componentModel = "spring")
public interface MoldDeliverProgressMapper {
    @Mapping(target = "progressId", source = "id")
    @Mapping(target = "moldSize", source = "mold.size")
    @Mapping(target = "moldSizeWithType", source = "entity", qualifiedByName = "getMoldSizeWithType")
    @Mapping(target = "canCheck", constant = "true")
    @Mapping(target = "actionDate", source = "entity", qualifiedByName = "getActionDate")
    MoldDeliverProgressDTO toDTO(MoldDeliverProgress entity);

    List<MoldDeliverProgressDTO> toDTOs(List<MoldDeliverProgress> entities);

    @Named("getMoldSizeWithType")
    default String getMoldSizeWithType(MoldDeliverProgress entity) {
        if (Objects.nonNull(entity.getMold())) {
            if (Objects.nonNull(entity.getMold().getMoldGroup())) {
                return entity.getMold().getSize()
                        + "#"
                        + entity.getMold().getMoldGroup().getType().name();
            }
            return entity.getMold().getSize();
        }
        return StringUtils.EMPTY;
    }

    @Named("getActionDate")
    default LocalDateTime getActionDate(MoldDeliverProgress entity) {
        return (Boolean.TRUE.equals(entity.getIsCompleted()) ? entity.getUpdatedDate() : null);
    }
}
