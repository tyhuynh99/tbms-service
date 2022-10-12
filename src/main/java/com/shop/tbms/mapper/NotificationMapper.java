package com.shop.tbms.mapper;

import com.shop.tbms.dto.NotificationDTO;
import com.shop.tbms.entity.TbmsNotification;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.ocpsoft.prettytime.PrettyTime;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Locale;

@Mapper(componentModel = "spring")
public interface NotificationMapper {
    PrettyTime PRETTY_TIME = new PrettyTime(new Locale("vi", "VN"));

    @Mapping(target = "createdDateDisplay", source = "createdDate", qualifiedByName = "genCreatedDateDisplay")
    NotificationDTO toDTO(TbmsNotification entity);

    List<NotificationDTO> toDTOs(List<TbmsNotification> entities);

    @Named("genCreatedDateDisplay")
    default String genCreatedDateDisplay(LocalDateTime createdDate) {
        return PRETTY_TIME.format(createdDate);
    }
}
