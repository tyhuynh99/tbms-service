package com.shop.tbms.component;

import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.PurchaseOrder;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class MoldComponent {
    public List<Mold> generateMoldEntity(List<String> listMoldReq, PurchaseOrder order) {
        return listMoldReq.stream().map(moldSizeReq -> {
            Mold mold = new Mold();
            mold.setPurchaseOrder(order);
            mold.setSize(moldSizeReq);

            return mold;
        }).collect(Collectors.toList());
    }
}
