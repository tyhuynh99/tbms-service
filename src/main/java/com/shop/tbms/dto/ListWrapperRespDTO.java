package com.shop.tbms.dto;

import lombok.*;

import java.util.List;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ListWrapperRespDTO<X> {
    List<X> data;
}
