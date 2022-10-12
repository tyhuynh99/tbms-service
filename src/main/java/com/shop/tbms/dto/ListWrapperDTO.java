package com.shop.tbms.dto;

import lombok.*;

import java.util.List;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ListWrapperDTO<X> {
    List<X> data;
}
