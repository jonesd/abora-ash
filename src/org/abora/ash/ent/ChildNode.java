/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.ent;

import java.util.ArrayList;
import java.util.List;

/**
 */
public abstract class ChildNode extends EntNode {
	//@todo should this be a set?
	protected List parents = new ArrayList();

	protected ChildNode(SequenceNumber branch) {
		super(branch);
	}

	protected void addToParent(EntNode node) {
		parents.add(node);
	}
	protected void removeFromParent(EntNode node) {
		parents.remove(node);
	}
	public List getParents() {
		return parents;
	}
	
	protected int dspForChild(EntNode node) {
		throw new UnsupportedOperationException();
	}


}
