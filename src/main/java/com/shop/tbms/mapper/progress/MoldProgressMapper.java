package com.shop.tbms.mapper.progress;

import com.shop.tbms.dto.step.detail.progress.MoldProgressDTO;
import com.shop.tbms.entity.MoldProgress;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;
import java.util.Objects;

@Mapper(componentModel = "spring")
public interface MoldProgressMapper {
    @Mapping(target = "progressId", source = "id")
    @Mapping(target = "moldId", source = "mold.id")
    @Mapping(target = "moldSize", source = "mold.size")
    @Mapping(target = "moldSizeWithType", source = "moldProgress", qualifiedByName = "getMoldSizeWithType")
    @Mapping(target = "canCheck", constant = "false")
    @Mapping(target = "canUncheck", constant = "false")
    MoldProgressDTO toDTO(MoldProgress moldProgress);

    List<MoldProgressDTO> toDTOs(List<MoldProgress> moldProgresses);

    @Named("getMoldSizeWithType")
    default String getMoldSizeWithType(MoldProgress moldProgress) {
        if (Objects.nonNull(moldProgress.getMold())) {
            if (Objects.nonNull(moldProgress.getMold().getMoldGroup())) {
                return moldProgress.getMold().getSize()
                        + "#"
                        + moldProgress.getMold().getMoldGroup().getType().name();
            }
            return moldProgress.getMold().getSize();
        }
        return StringUtils.EMPTY;
    }
}
