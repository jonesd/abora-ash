/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.ent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 */
public abstract class LeafNode extends ChildNode {
	protected int startPosition = 0;

	protected LeafNode(SequenceNumber branch, int startPosition) {
		super(branch);
		this.startPosition = startPosition;
	}

	//	"Filed out from Dolphin Smalltalk 2002 release 5.00"!
	//
	//	ChildNode subclass: #LeafNode
	//		instanceVariableNames: 'startPosition'
	//		classVariableNames: ''
	//		poolDictionaries: ''
	//		classInstanceVariableNames: ''!
	//	LeafNode guid: (GUID fromString: '{F64D0B1E-54DA-4D5C-97C8-665909D3718D}')!
	//	LeafNode comment: ''!
	//	!LeafNode categoriesForClass!Kernel-Objects! !
	//	!LeafNode methodsFor!
	//

	protected void basicInsertChild(LeafNode leaf, int position, RootNode root) {
		int index = position - startPosition + 1;
		if (index < 0 || index > count()) {
			throw new IndexOutOfBoundsException(String.valueOf(position));
		}
		if (index == 0) {
			// Insert before
			if (isMinNodeFor(root.getBranch())) {
				basicInsertBeforeEnt(leaf, root);
			} else {
				basicInsertBeforeSelf(leaf, position, root);
			}
		} else {
			if (index < count()) {
				//TODO split then insert
				SplitNode splitNode = replaceWithSplit(position, index);
				splitNode.getRight().insertLeaf(leaf, position, root);
			} else {
				if (isMaxNodeFor(root.getBranch())) {
					basicInsertAfterEnt(leaf, root);
				} else {
					throw new IndexOutOfBoundsException(String.valueOf(position));
				}
			}
		}
	}

	//	basicInsert: dataLeaf at: position root: root
	//		| index |
	//		index := position - startPosition + 1.
	//		(index < 1 or: [index > (self count + 1)]) ifTrue: [self errorSubscriptBounds: position].
	//		index = 1 
	//			ifTrue: 
	//				["insert before"
	//
	//				(self isMinNodeFor: root branch) 
	//					ifTrue: [self basicInsertBeforeEnt: dataLeaf root: root]
	//					ifFalse: 
	//						[^self 
	//							basicInsertBeforeSelf: dataLeaf
	//							at: position
	//							root: root]]
	//			ifFalse: 
	//				[index <= self count 
	//					ifTrue: 
	//						["split then insert"
	//
	//						| splitNode |
	//						splitNode := self replaceWithSplit: position about: index.
	//						^splitNode right 
	//							insert: dataLeaf
	//							at: position
	//							root: root]
	//					ifFalse: 
	//						[(self isMaxNodeFor: root branch) 
	//							ifTrue: [self basicInsertAfterEnt: dataLeaf root: root]
	//							ifFalse: [self errorSubscriptBounds: position]]]!
	//

	protected void basicInsertAfterEnt(LeafNode leaf, RootNode root) {
		SplitNode splitNode = new SplitNode(root.getBranch(), leaf.getStartPosition(), root.getChild(), root.getDsp(), leaf);
		root.setChild(splitNode);
		root.setDspForChild(0, splitNode);
	}

	//	basicInsertAfterEnt: dataLeaf root: root
	//		"add to end of entire ent"
	//
	//		| splitNode |
	//		splitNode := SplitNode 
	//					branch: root branch
	//					split: dataLeaf startPosition
	//					left: root child
	//					leftDsp: root dsp
	//					right: dataLeaf.
	//		root child: splitNode.
	//		root dsp: 0!
	//

	protected void basicInsertBeforeEnt(LeafNode leaf, RootNode root) {
		SplitNode splitNode =
			new SplitNode(root.getBranch(), leaf.count() + leaf.getStartPosition(), leaf, root.getChild(), leaf.count() + root.getDsp());
		root.setChild(splitNode);
		root.setDspForChild(0, splitNode);
	}

	//	basicInsertBeforeEnt: dataLeaf root: root
	//		"add to beginning of entire ent"
	//
	//		| splitNode |
	//		splitNode := SplitNode 
	//					branch: root branch
	//					split: dataLeaf count + dataLeaf startPosition
	//					left: dataLeaf
	//					right: root child
	//					rightDsp: dataLeaf count + root dsp.
	//		root child: splitNode.
	//		root dsp: 0!
	//

	protected void basicInsertBeforeSelf(LeafNode leaf, int position, RootNode root) {
		SequenceNumber newBranch = root.getBranch();
		SplitNode parent = parentSplit(newBranch);
		assert parent != null;
		assert parent.getLeft() == this || parent.getRight() == this;
		if (parent.getLeft() == this || parent.parentSplit(newBranch) == null) {
			SplitNode rootChild = (SplitNode) root.getChild();
			if (rootChild.isSameFor(newBranch)) {
				rootChild.getRight().removeFromParent(rootChild);
			}
			SplitNode a =
				new SplitNode(
					newBranch,
					leaf.getStartPosition() + leaf.count(),
					leaf,
					rootChild.getRight(),
					leaf.count() + rootChild.getRightDsp() + root.getDsp());
			if (rootChild.isSameFor(newBranch)) {
				rootChild.getLeft().removeFromParent(root.getChild());
			}
			SplitNode b = new SplitNode(newBranch, leaf.getStartPosition(), rootChild.getLeft(), rootChild.getLeftDsp() + root.getDsp(), a);
			root.setChild(b);
			root.setDsp(0);
		} else {
			// Force self to other side of parent by re-splaying
			parent.basicSplayFor(newBranch);
			basicInsertChild(leaf, position, root);
		}
	}

	//	basicInsertBeforeSelf: dataLeaf at: position root: root
	//		"add before self"
	//
	//		| parent newBranch |
	//		newBranch := root branch.
	//		parent := self parentSplit: newBranch.
	//		self assert: [parent notNil].
	//		self assert: [parent left == self or: [parent right == self]].
	//		(parent left == self or: [(parent parentSplit: newBranch) isNil]) 
	//			ifTrue: 
	//				[| a b |
	//				(root child isSameFor: newBranch) ifTrue: [root child right removeFromParent: root child].
	//				a := SplitNode 
	//							branch: newBranch
	//							split: dataLeaf startPosition + dataLeaf count
	//							left: dataLeaf
	//							right: root child right
	//							rightDsp: dataLeaf count + root child rightDsp + root dsp.
	//				(root child isSameFor: newBranch) ifTrue: [root child left removeFromParent: root child].
	//				b := SplitNode 
	//							branch: newBranch
	//							split: dataLeaf startPosition
	//							left: root child left
	//							leftDsp: root child leftDsp + root dsp
	//							right: a.
	//				root child: b.
	//				root dsp: 0]
	//			ifFalse: 
	//				["Force self to other side of parent by splaying"
	//
	//				parent basicSplayFor: newBranch.
	//				^self 
	//					basicInsert: dataLeaf
	//					at: position
	//					root: root]!
	//

	protected EntNode basicSplayFor(SequenceNumber matchingBranch) {
		return singleParentFor(matchingBranch).basicSplayFor(matchingBranch);
	}

	//	basicSplayFor: matchingBranch
	//		^(self singleParentFor: matchingBranch) basicSplayFor: matchingBranch!
	//

	public List children() {
		return Collections.EMPTY_LIST;
	}

	//	children
	//		^#()
	//	!
	//
	//	constrainedIndexFrom: position
	//		| index |
	//		index := position - self startPosition + 1.
	//		(index < 1 or: [index > self count]) ifTrue: [self errorSubscriptBounds: index].
	//		^index!
	//
	//	contents
	//		^self contentsFrom: self startPosition extent: self count!
	//
	//	contentsAt: position
	//		| index |
	//		index := self constrainedIndexFrom: position.
	//		^self contentsAtConstrainedIndex: index!
	//
	//	contentsAtConstrainedIndex: index
	//		self subclassResponsibility!
	//

	public EntNode duplicateFor(SequenceNumber newBranch) {
		throw new UnsupportedOperationException("duplicateFor");

	}

	//	duplicateFor: newBranch
	//		self shouldNotImplement!
	//
	//	firstNodeFrom: position extent: extent shouldSplit: shouldSplit
	//		| index |
	//		index := self constrainedIndexFrom: position.
	//		shouldSplit ifFalse: [^self].
	//		^index > 1 
	//			ifTrue: 
	//				[| newParent |
	//				newParent := self replaceWithSplit: position about: index.
	//				newParent right 
	//					firstNodeFrom: position
	//					extent: extent
	//					shouldSplit: shouldSplit]
	//			ifFalse: 
	//				[extent < self count 
	//					ifTrue: 
	//						[| newParent |
	//						newParent := self replaceWithSplit: position + extent about: extent + 1.
	//						newParent left]
	//					ifFalse: [self]]!
	//
	//	globalPositionFor: matchingBranch
	//		^self globalPositionFor: matchingBranch to: nil!
	//
	//	globalPositionFor: matchingBranch to: topParent
	//		| position node parent |
	//		position := self startPosition.
	//		node := self.
	//		[(parent := node singleParentFor: matchingBranch) ~~ topParent] whileTrue: 
	//				[position := position + (parent dspForChild: node).
	//				node := parent].
	//		^position!
	//
	//	globalRegionFor: forBranch
	//		^IntegerRegion startPosition: (self globalPositionFor: forBranch) extent: self count!
	//

	public void insertLeaf(LeafNode leaf, int position, RootNode root) {
		int index = position - startPosition + 1;
		if (index < 0 || index > count()) {
			throw new IndexOutOfBoundsException(String.valueOf(position));
		}
		basicSplayFor(root.getBranch());
		basicInsertChild(leaf, position, root);
	}

	//	insert: dataLeaf at: position root: root
	//		| index |
	//		index := position - startPosition + 1.
	//		(index < 1 or: [index > (self count + 1)]) ifTrue: [self errorSubscriptBounds: position].
	//		self basicSplayFor: root branch.
	//		self 
	//			basicInsert: dataLeaf
	//			at: position
	//			root: root!
	//

	public boolean isMaxNodeFor(SequenceNumber matchingBranch) {
		EntNode node = this;
		SplitNode splitParent;
		while ((splitParent = node.parentSplit(matchingBranch)) != null) {
			if (splitParent.getRight() != node) {
				return false;
			}
			node = splitParent;
		}
		return true;
	}

	//	isMaxNodeFor: matchingBranch
	//		| node parent |
	//		node := self.
	//		[(parent := node parentSplit: matchingBranch) notNil] whileTrue: 
	//				[parent right == node ifFalse: [^false].
	//				node := parent].
	//		^true!
	//

	public boolean isMinNodeFor(SequenceNumber matchingBranch) {
		EntNode node = this;
		SplitNode splitParent;
		while ((splitParent = node.parentSplit(matchingBranch)) != null) {
			if (splitParent.getLeft() != node) {
				return false;
			}
			node = splitParent;
		}
		return true;
	}

	//	isMinNodeFor: matchingBranch
	//		| node parent |
	//		node := self.
	//		[(parent := node parentSplit: matchingBranch) notNil] whileTrue: 
	//				[parent left == node ifFalse: [^false].
	//				node := parent].
	//		^true!
	//

	public EntNode maxNode() {
		return this;
	}

	//	maxNode
	//		^self!
	//

	public EntNode minNode() {
		return this;
	}

	//	minNode
	//		^self!
	//
	//	nodeAt: position
	//		| index |
	//		index := self constrainedIndexFrom: position.
	//		^self!
	//
	//	removeFor: newBranch
	//		self basicSplayFor: newBranch.
	//		(self singleParentFor: newBranch) removeChild: self branch: newBranch!
	//

	protected SplitNode replaceWithSplit(int newSplit, int elementsPosition) {
		SplitNode splitNode = splitAbout(newSplit, elementsPosition);
		List parentsCopy = new ArrayList(getParents());
		for (Iterator iter = parentsCopy.iterator(); iter.hasNext();) {
			EntNode parent = (EntNode) iter.next();
			parent.replaceChild(this, splitNode);
		}
		assert getParents().isEmpty();
		return splitNode;
	}

	//	replaceWithSplit: newSplit about: elementsPosition
	//		"Answer a new SplitNode with two data children with elements before and equal and after elementsPosition.
	//		NOTE: This operation is unusual in that all parent versions are redirected to point to the replacement split node."
	//
	//		| splitNode |
	//		splitNode := self split: newSplit about: elementsPosition.
	//		self parents do: [:parent | parent replaceChild: self with: splitNode].
	//		self assert: [self parents isEmpty].
	//		^splitNode!
	//

	protected abstract SplitNode splitAbout(int newSplit, int elementsPosition);

	//	split: newSplit about: elementsPosition
	//		self subclassResponsibility!
	//

	public int getStartPosition() {
		return startPosition;
	}

	//	startPosition
	//		^startPosition!
	//
	//	startPosition: anObject
	//		startPosition := anObject! !
	//	!LeafNode categoriesFor: #basicInsert:at:root:!private! !
	//	!LeafNode categoriesFor: #basicInsertAfterEnt:root:!private! !
	//	!LeafNode categoriesFor: #basicInsertBeforeEnt:root:!private! !
	//	!LeafNode categoriesFor: #basicInsertBeforeSelf:at:root:!private! !
	//	!LeafNode categoriesFor: #basicSplayFor:!private! !
	//	!LeafNode categoriesFor: #children!public! !
	//	!LeafNode categoriesFor: #constrainedIndexFrom:!private! !
	//	!LeafNode categoriesFor: #contents!public! !
	//	!LeafNode categoriesFor: #contentsAt:!public! !
	//	!LeafNode categoriesFor: #contentsAtConstrainedIndex:!public! !
	//	!LeafNode categoriesFor: #duplicateFor:!public! !
	//	!LeafNode categoriesFor: #firstNodeFrom:extent:shouldSplit:!public! !
	//	!LeafNode categoriesFor: #globalPositionFor:!public! !
	//	!LeafNode categoriesFor: #globalPositionFor:to:!private! !
	//	!LeafNode categoriesFor: #globalRegionFor:!public! !
	//	!LeafNode categoriesFor: #insert:at:root:!public! !
	//	!LeafNode categoriesFor: #isMaxNodeFor:!public! !
	//	!LeafNode categoriesFor: #isMinNodeFor:!public! !
	//	!LeafNode categoriesFor: #maxNode!public! !
	//	!LeafNode categoriesFor: #minNode!public! !
	//	!LeafNode categoriesFor: #nodeAt:!public! !
	//	!LeafNode categoriesFor: #removeFor:!public! !
	//	!LeafNode categoriesFor: #replaceWithSplit:about:!public! !
	//	!LeafNode categoriesFor: #split:about:!public! !
	//	!LeafNode categoriesFor: #startPosition!accessing!private! !
	//	!LeafNode categoriesFor: #startPosition:!accessing!private! !
	//
	//
	//
	protected void replaceChild(EntNode existingChild, EntNode newChild) {
		throw new UnsupportedOperationException("no children");
	}

	protected void setDspForChild(int dsp, EntNode child) {
		throw new UnsupportedOperationException("no children");
	}

}
