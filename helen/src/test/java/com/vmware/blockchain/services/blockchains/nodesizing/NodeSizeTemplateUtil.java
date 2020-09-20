/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.nodesizing;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * Utility class for NodeSizeTemplate tests.
 */
public class NodeSizeTemplateUtil {

    /**
     * Create and return a NodeSizeTemplate object.
     * @param id Id for the NodeSizeTemplate object
     * @param name Name of the NodeSizeTemplate object.
     * @return An instance of NodeSizeTemplate object.
     */
    public static NodeSizeTemplate createNodeSizeTemplate(UUID id, String name) {
        final NodeSizeTemplate nst = new NodeSizeTemplate(id, name);

        final List<NodeSizeTemplate.Template> templates = new ArrayList<>();

        NodeSizeTemplate.Template t1 = new NodeSizeTemplate.Template();
        t1.setName("Small");

        NodeSizeTemplate.Item item1 = new NodeSizeTemplate.Item();
        item1.put(NodeSizeTemplate.Parameter.TYPE, "Client");
        item1.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, "2");
        item1.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, "32");
        item1.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, "250");

        List<NodeSizeTemplate.Item> items = new ArrayList<>();
        items.add(item1);

        NodeSizeTemplate.Item item2 = new NodeSizeTemplate.Item();
        item2.put(NodeSizeTemplate.Parameter.TYPE, "replica");
        item2.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, "2");
        item2.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, "32");
        item2.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, "250");
        items.add(item2);
        t1.setItems(items);

        templates.add(t1);
        items.clear();

        NodeSizeTemplate.Template t2 = new NodeSizeTemplate.Template();
        t2.setName("Medium");

        NodeSizeTemplate.Item item3 = new NodeSizeTemplate.Item();
        item3.put(NodeSizeTemplate.Parameter.TYPE, "Client");
        item3.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, "2");
        item3.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, "32");
        item3.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, "250");
        items.add(item3);

        NodeSizeTemplate.Item item4 = new NodeSizeTemplate.Item();
        item4.put(NodeSizeTemplate.Parameter.TYPE, "replica");
        item4.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, "2");
        item4.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, "32");
        item4.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, "250");
        items.add(item4);

        templates.add(t2);
        items.clear();

        NodeSizeTemplate.Template t3 = new NodeSizeTemplate.Template();
        t3.setName("Large");
        NodeSizeTemplate.Item item5 = new NodeSizeTemplate.Item();
        item5.put(NodeSizeTemplate.Parameter.TYPE, "Client");
        item5.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, "2");
        item5.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, "32");
        item5.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, "250");
        items.add(item5);

        NodeSizeTemplate.Item item6 = new NodeSizeTemplate.Item();
        item6.put(NodeSizeTemplate.Parameter.TYPE, "replica");
        item6.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, "2");
        item6.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, "32");
        item6.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, "250");
        items.add(item6);

        templates.add(t3);

        nst.setTemplates(templates);

        final NodeSizeTemplate.Range range = new NodeSizeTemplate.Range();

        NodeSizeTemplate.RangeVal rangeVal1 = new NodeSizeTemplate.RangeVal();
        rangeVal1.put(NodeSizeTemplate.RangeProperty.MIN, 1);
        rangeVal1.put(NodeSizeTemplate.RangeProperty.MAX, 16);
        range.put(NodeSizeTemplate.Parameter.NO_OF_CPUS, rangeVal1);

        NodeSizeTemplate.RangeVal rangeVal2 = new NodeSizeTemplate.RangeVal();
        rangeVal2.put(NodeSizeTemplate.RangeProperty.MIN, 1);
        rangeVal2.put(NodeSizeTemplate.RangeProperty.MAX, 256);
        range.put(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS, rangeVal2);

        NodeSizeTemplate.RangeVal rangeVal3 = new NodeSizeTemplate.RangeVal();
        rangeVal3.put(NodeSizeTemplate.RangeProperty.MIN, 1);
        rangeVal3.put(NodeSizeTemplate.RangeProperty.MAX, 1000);
        range.put(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS, rangeVal3);

        nst.setRange(range);

        return nst;
    }
}
