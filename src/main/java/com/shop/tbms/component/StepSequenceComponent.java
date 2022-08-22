package com.shop.tbms.component;

import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.StepSequence;
import com.shop.tbms.entity.TemplateStepSequence;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class StepSequenceComponent {
    public List<StepSequence> generateStepSequence(PurchaseOrder currentPurchaseOrder, List<TemplateStepSequence> listTemplateStepSequence) {
        /* Generate StepSequence from template */

        /* Map Step ID to StepSequence */
        return null;
    }
}
