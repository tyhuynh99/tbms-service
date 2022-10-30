package com.shop.tbms.dto;

import lombok.*;

import java.util.List;

@Getter @Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ListWrapperDTO<X> {
    List<X> data;
}
