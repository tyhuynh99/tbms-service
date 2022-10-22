package com.shop.tbms.util;

import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldGroup;
import org.apache.commons.lang3.StringUtils;

import java.util.Optional;

public class MoldUtil {
    public static String getMoldName(Mold mold) {
        return mold.getSize()
                + Optional.ofNullable(mold.getMoldGroup())
                .map(MoldGroup::getType)
                .map(moldType -> "#" + moldType.name())
                .orElse(StringUtils.EMPTY);
    }
}
