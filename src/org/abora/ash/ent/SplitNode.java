/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.ent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 */
public class SplitNode extends ChildNode {
	private int split = 0;
	private EntNode left = null;
	private EntNode right = null;
	private int leftDsp = 0;
	private int rightDsp = 0;

	public SplitNode(SequenceNumber branch, int split, EntNode left, int leftDsp, EntNode right) {
		this(branch, split, left, leftDsp, right, 0);
	}
	public SplitNode(SequenceNumber branch, int split, EntNode left, int leftDsp, EntNode right, int rightDsp) {
		super(branch);
		this.split = split;
		this.left = left;
		this.leftDsp = leftDsp;
		this.right = right;
		this.rightDsp = rightDsp;
	}
	public SplitNode(SequenceNumber branch, int split, EntNode left, EntNode right) {
		this(branch, split, left, 0, right, 0);
	}
	public SplitNode(SequenceNumber branch, int split, EntNode left, EntNode right, int rightDsp) {
		this(branch, split, left, 0, right, rightDsp);
	}

	private int applyDsp(int dsp, int position) {
		return position + invertDsp(dsp);
	}

	protected void assertIsChild(EntNode node) {
		if (left != node && right != node) {
			throw new IllegalStateException("unknown child");
		}
	}

	//	assertIsChild: node
	//		(left ~~ node and: [right ~~ node]) ifTrue: [EntError signal: 'unknown child'].
	//		#todo	"ensure left !!= right"!

	protected SplitNode basicParentSplit() {
		return this;
	}

	protected EntNode basicSplayFor(SequenceNumber matchingBranch) {
		SplitNode parent = parentSplit(matchingBranch);
		if (parent == null) {
			return this;
		}
		SplitNode grandParent = parent.parentSplit(matchingBranch);
		EntNode newSelf;
		if (parent.getLeft() == this) {
			if (grandParent != null && parent == grandParent.getLeft()) {
				parent = (SplitNode)grandParent.singleRotateLeftFor(matchingBranch);
			}
			newSelf = parent.singleRotateLeftFor(matchingBranch);
		} else {
			if (grandParent != null && parent == grandParent.getRight()) {
				parent = (SplitNode)grandParent.singleRotateRightFor(matchingBranch);
			}
			newSelf = parent.singleRotateRightFor(matchingBranch);
		}
		newSelf.basicSplayFor(matchingBranch);
		return newSelf;
	}

	//	basicSplayFor: matchingBranch
	//		| parent grandParent newSelf |
	//		parent := self parentSplit: matchingBranch.
	//		parent isNil ifTrue: [^self].
	//		grandParent := parent parentSplit: matchingBranch.
	//		newSelf := parent left == self 
	//					ifTrue: 
	//						[(grandParent notNil and: [parent = grandParent left]) 
	//							ifTrue: [parent := grandParent singleRotateLeftFor: matchingBranch].
	//						parent singleRotateLeftFor: matchingBranch]
	//					ifFalse: 
	//						[(grandParent notNil and: [parent = grandParent right]) 
	//							ifTrue: [parent := grandParent singleRotateRightFor: matchingBranch].
	//						parent singleRotateRightFor: matchingBranch].
	//		newSelf basicSplayFor: matchingBranch.
	//		^newSelf!
	//
	//	childFor: position do: duadic
	//		| child dsp dspPosition |
	//		(self isLeft: position) 
	//			ifTrue: 
	//				[child := self left.
	//				dsp := self leftDsp]
	//			ifFalse: 
	//				[child := self right.
	//				dsp := self rightDsp].
	//		dspPosition := self applyDsp: dsp to: position.
	//		^duadic value: child value: dspPosition.!
	//

	public List children() {
		List list = new ArrayList(2);
		list.add(left);
		list.add(right);
		return list;
	}

	//	children
	//		^Array with: self left with: self right
	//	!
	//
	//	contents
	//		| minElement position |
	//		minElement := self minNode.
	//		position := minElement globalPositionFor: self branch
	//					to: (self singleParentFor: self branch).
	//		^self contentsFrom: position extent: self count!
	//
	//	contentsAt: position
	//		^self childFor: position do: [:child :dspPosition | child contentsAt: dspPosition]!
	//
	//	contentsFrom: position extent: extent do: operation
	//		position < self split ifTrue: [self left contentsFrom: (self applyDsp: self leftDsp to: position) extent: extent do: operation].
	//		position + extent > self split ifTrue: [self right contentsFrom: (self applyDsp: self rightDsp to: position) extent: extent do: operation].
	//	!
	//
	//	contentsFrom: position extent: extent into: stream
	//		position < self split ifTrue: [self left contentsFrom: (self applyDsp: self leftDsp to: position) extent: extent into: stream].
	//		position + extent > self split ifTrue: [self right contentsFrom: (self applyDsp: self rightDsp to: position) extent: extent into: stream].
	//	!

	public int count() {
		return left.count() + right.count();
	}

	public void setDspForChild(int dsp, EntNode node) {
		assertIsChild(node);
		if (left == node) {
			leftDsp = dsp;
		} else if (right == node) {
			rightDsp = dsp;
		}
	}

	//	dsp: dsp forChild: node
	//		self assertIsChild: node.
	//		left == node ifTrue: [self leftDsp: dsp].
	//		right == node ifTrue: [self rightDsp: dsp]!
	//

	public int dspForChild(EntNode node) {
		if (left == node) {
			return getLeftDsp();
		} else if (right == node) {
			return getRightDsp();
		} else {
			throw new IllegalArgumentException("unknown child");
		}
	}

	//	dspForChild: node
	//		self assertIsChild: node.
	//		left == node ifTrue: [^self leftDsp].
	//		right == node ifTrue: [^self rightDsp]!
	//

	public EntNode duplicateFor(SequenceNumber duplicateBranch) throws NonCompatibleBranchException {
		ensureCompatibleFor(duplicateBranch);
		//TODO perhaps return this; in this case?
		if (isSameFor(duplicateBranch)) {
			return this;
		}
		return new SplitNode(duplicateBranch, getSplit(), getLeft(), getLeftDsp(), getRight(), getRightDsp());
	}

	//	duplicateFor: duplicateBranch
	//		"Answer a copy of the receiver if self isn't of the required revision.
	//		The duplicate connects to self children, but has no parents set."
	//
	//		self ensureCompatibleFor: duplicateBranch.
	//		#todo.	"perhaps ^self in this case?"
	//		(self isSameFor: duplicateBranch) ifTrue: [^self].
	//		^self class 
	//			branch: duplicateBranch
	//			split: self split
	//			left: self left
	//			leftDsp: self leftDsp
	//			right: self right
	//			rightDsp: self rightDsp!
	//
	//	firstNodeFrom: position extent: extent shouldSplit: shouldSplit
	//		^self childFor: position
	//			do: 
	//				[:child :dspPosition | 
	//				child 
	//					firstNodeFrom: dspPosition
	//					extent: extent
	//					shouldSplit: shouldSplit]!
	//

	public void insertLeaf(LeafNode leaf, int position, RootNode root) {
		throw new UnsupportedOperationException("todo");

	}

	//	insert: dataLeaf at: position root: root
	//		^self childFor: position do: [:child :dspPosition | child insert: dataLeaf at: dspPosition root: root].!
	//

	protected int invertDsp(int dsp) {
		return -dsp;
	}

	//	invertDsp: dsp
	//		^dsp negated!
	//
	
	public boolean isLeft(int position) {
		return position < getSplit();
	}
	
	//	isLeft: position
	//		^position < self split!

	public EntNode getLeft() {
		return left;
	}

	public void setLeft(EntNode node) {
		if (left == node)
			return;
		if (left != null)
			left.removeFromParent(this);
		left = node;
		node.addToParent(this);
	}

	public int getLeftDsp() {
		return leftDsp;
	}
	
	public void setLeftDsp(int leftDsp) {
		this.leftDsp = leftDsp;
	}

	//	nodeAt: position
	//		^self childFor: position do: [:child :dspPosition | child nodeAt: dspPosition]!
	//
	//	removeChild: existingChild branch: newBranch
	//		| parent duplicate |
	//		self assertIsChild: existingChild.
	//		(right == existingChild and: [(right isMaxNodeFor: newBranch) not]) 
	//			ifTrue: 
	//				["Splay to toggle childs location into one that we can handle"
	//
	//				^existingChild removeFor: newBranch].
	//		"duplicate self and parents chain to root, then just use the duplicates"
	//		#todo.	" only need to duplicate if node is being shared with outher versions"
	//		duplicate := (self isSameFor: newBranch) 
	//					ifTrue: 
	//						["existingChild"
	//
	//						self]
	//					ifFalse: [self duplicateWithParentsFor: newBranch].
	//		parent := duplicate singleParentFor: newBranch.
	//		duplicate left == existingChild 
	//			ifTrue: 
	//				[parent == (duplicate rootFor: newBranch) 
	//					ifTrue: [duplicate removeChildLeftFromRoot]
	//					ifFalse: [duplicate removeChildLeft]]
	//			ifFalse: 
	//				[self assert: [duplicate right == existingChild].
	//				duplicate removeChildRightMaxElement]!

	protected void removeChildLeft() throws NonCompatibleBranchException {
		// Assumed to be a duplicate by this point
		EntNode parent = singleParentFor(branch);
		parent.setDspForChild(parent.dspForChild(this) + rightDsp, this);
		parent.replaceChild(this, right);
		right.removeFromParent(this);
		SplitNode rootChild = (SplitNode) right.rootFor(branch).getChild();
		rootChild.rightDsp = rootChild.rightDsp - left.count();
	}
	//	removeChildLeft
	//		"Assumed to be a duplicate by this point"
	//
	//		| parent rootChild |
	//		parent := self singleParentFor: self branch.
	//		parent dsp: (parent dspForChild: self) + self rightDsp forChild: self.
	//		parent replaceChild: self with: self right.
	//		self right removeFromParent: self.
	//		rootChild := (self right rootFor: self branch) child.
	//		rootChild rightDsp: rootChild rightDsp - self left count!

	protected void removeChildLeftFromRoot() {
		EntNode parent = singleParentFor(branch);
		parent.setDspForChild(parent.dspForChild(this) + rightDsp - left.count(), this);
		parent.replaceChild(this, right);
		right.removeFromParent(this);
	}
	//	removeChildLeftFromRoot
	//		| parent |
	//		parent := self singleParentFor: self branch.
	//		parent dsp: (parent dspForChild: self) + self rightDsp - self left count forChild: self.
	//		parent replaceChild: self with: self right.
	//		self right removeFromParent: self!

	protected void removeChildRightMaxElement() {
		EntNode parent = singleParentFor(getBranch());
		parent.setDspForChild(parent.dspForChild(this) + getLeftDsp(), this);
		parent.replaceChild(this, getLeft());
		getLeft().removeFromParent(this);
	}

	//	removeChildRightMaxElement
	//		| parent |
	//		parent := self singleParentFor: self branch.
	//		parent dsp: (parent dspForChild: self) + self leftDsp forChild: self.
	//		parent replaceChild: self with: self left.
	//		self left removeFromParent: self!
	//

	protected void replaceChild(EntNode existingChild, EntNode newChild) {
		assertIsChild(existingChild);
		if (left == existingChild) {
			setLeft(newChild);
		} else if (right == newChild) {
			setRight(newChild);
		}
	}

	//	replaceChild: existingChild with: newChild
	//		self assertIsChild: existingChild.
	//		left == existingChild ifTrue: [self left: newChild].
	//		right == existingChild ifTrue: [self right: newChild]!
	//
	//	right
	//		^right!

	public EntNode getRight() {
		return right;
	}

	public void setRight(EntNode node) {
		if (right == node)
			return;
		if (right != null)
			right.removeFromParent(this);
		right = node;
		node.addToParent(this);
	}

	//
	//	right: aNode
	//		right == aNode ifTrue: [^self].
	//		right notNil ifTrue: [right removeFromParent: self].
	//		right := aNode.
	//		aNode addToParent: self!
	//
	//	rightDsp
	//		^rightDsp!

	public int getRightDsp() {
		return rightDsp;
	}
	
	public void setRightDsp(int rightDsp) {
		this.rightDsp = rightDsp;
	}

	//
	//	rightDsp: anObject
	//		rightDsp := anObject!
	//

	protected EntNode singleRotateLeftFor(SequenceNumber matchingBranch) {
		EntNode node2 = getLeft();
		assert node2 instanceof SplitNode;
		//TODO mess!! + dont have to duplicate every different root branching off at this point
		if (!isSameFor(node2.getBranch())) {
			assert node2.getBranch().isBranchingBefore(getBranch());
			for (Iterator iter = node2.getParents().iterator(); iter.hasNext();) {
				EntNode node2Parent = (EntNode) iter.next();
				if (node2Parent != this) {
					EntNode newNode2 = node2.duplicateFor(node2Parent.getBranch());
					node2Parent.replaceChild(node2, newNode2);
				}
			}
			EntNode myNewNode2 = node2.duplicateFor(getBranch());
			replaceChild(node2, myNewNode2);
			return singleRotateLeftFor(matchingBranch);
		}
		for (Iterator iter = node2.getParents().iterator(); iter.hasNext();) {
			EntNode node2Parent = (EntNode) iter.next();
			if (node2Parent != this) {
				EntNode newNode2 = node2.duplicateFor(node2Parent.getBranch());
				node2Parent.replaceChild(node2, newNode2);
			}
		}
		int b = getLeftDsp();
		int c = getRightDsp();
		int d = ((SplitNode)getLeft()).getLeftDsp();
		int e = ((SplitNode)getLeft()).getRightDsp();
		setLeft(((SplitNode)node2).getRight());
		for (Iterator iter = getParents().iterator(); iter.hasNext();) {
			EntNode parent = (EntNode) iter.next();
			int a = parent.dspForChild(this);
			parent.replaceChild(this, node2);
			parent.setDspForChild(a + b, node2);			
		}
		((SplitNode)node2).setRight(this);
		((SplitNode)node2).setLeftDsp(d);
		((SplitNode)node2).setRightDsp(-b);
		setLeftDsp(b + e);
		setRightDsp(c);
		return node2;
	}

	//	singleRotateLeftFor: matchingBranch
	//		| node2 b c d e |
	//		node2 := self left.
	//		self assert: [node2 isMemberOf: self class].
	//		#todo.	"mess!! + dont have to duplicate every different root branching off at this point"
	//		(self isSameFor: node2 branch) 
	//			ifFalse: 
	//				[| myNewNode2 |
	//				self assert: [node2 branch < self branch].
	//				node2 parents asArray do: 
	//						[:node2Parent | 
	//						| newNode2 |
	//						node2Parent ~~ self 
	//							ifTrue: 
	//								[newNode2 := node2 duplicateFor: node2Parent branch.
	//								node2Parent replaceChild: node2 with: newNode2]].
	//				myNewNode2 := node2 duplicateFor: self branch.
	//				self replaceChild: node2 with: myNewNode2.
	//				^self singleRotateLeftFor: matchingBranch].
	//		node2 parents asArray do: 
	//				[:node2Parent | 
	//				| newNode2 |
	//				node2Parent ~~ self 
	//					ifTrue: 
	//						[newNode2 := node2 duplicateFor: node2Parent branch.
	//						node2Parent replaceChild: node2 with: newNode2]].
	//		"parent := self singleParent: matchingRevision."
	//		"get existing dsp's"
	//		"a := parent dspForChild: self."
	//		b := self leftDsp.
	//		c := self rightDsp.
	//		d := self left leftDsp.
	//		e := self left rightDsp.
	//		"rotate left child (node2) to become  parent of self"
	//		self left: node2 right.
	//		"parent replaceChild: self with: node2."
	//		"update all dsps"
	//		self parents do: 
	//				[:parent | 
	//				| a |
	//				a := parent dspForChild: self.
	//				parent replaceChild: self with: node2.
	//				parent dsp: a + b forChild: node2].
	//		node2 right: self.
	//		node2 leftDsp: d.
	//		node2 rightDsp: b negated.
	//		self leftDsp: b + e.
	//		self rightDsp: c.
	//		^node2!
	//
	protected EntNode singleRotateRightFor(SequenceNumber matchingBranch) {
		EntNode node2 = getRight();
		assert node2 instanceof SplitNode;
		//TODO mess!! + dont have to duplicate every different root branching off at this point
		if (!isSameFor(node2.getBranch())) {
			assert node2.getBranch().isBranchingBefore(getBranch());
			for (Iterator iter = node2.getParents().iterator(); iter.hasNext();) {
				EntNode node2Parent = (EntNode) iter.next();
				if (node2Parent != this) {
					EntNode newNode2 = node2.duplicateFor(node2Parent.getBranch());
					node2Parent.replaceChild(node2, newNode2);
				}
			}
			EntNode myNewNode2 = node2.duplicateFor(getBranch());
			replaceChild(node2, myNewNode2);
			return singleRotateRightFor(matchingBranch);
		}
		for (Iterator iter = node2.getParents().iterator(); iter.hasNext();) {
			EntNode node2Parent = (EntNode) iter.next();
			if (node2Parent != this) {
				EntNode newNode2 = node2.duplicateFor(node2Parent.getBranch());
				node2Parent.replaceChild(node2, newNode2);
			}
		}
		int b = getLeftDsp();
		int c = getRightDsp();
		int d = ((SplitNode)getRight()).getLeftDsp();
		int e = ((SplitNode)getRight()).getRightDsp();
		setRight(((SplitNode)node2).getLeft());
		for (Iterator iter = getParents().iterator(); iter.hasNext();) {
			EntNode parent = (EntNode) iter.next();
			int a = parent.dspForChild(this);
			parent.replaceChild(this, node2);
			parent.setDspForChild(a + c, node2);			
		}
		((SplitNode)node2).setLeft(this);
		((SplitNode)node2).setLeftDsp(-c);
		((SplitNode)node2).setRightDsp(e);
		((SplitNode)((SplitNode)node2).getLeft()).setLeftDsp(b);
		((SplitNode)((SplitNode)node2).getLeft()).setRightDsp(c + d);
		return node2;
	}
	//	singleRotateRightFor: matchingBranch
	//		| node2 b c d e |
	//		node2 := self right.
	//		self assert: [node2 isMemberOf: self class].
	//		#todo.	"mess!! + dont have to duplicate every different root branching off at this point"
	//		(self isSameFor: node2 branch) 
	//			ifFalse: 
	//				[| myNewNode2 |
	//				self assert: [node2 branch < self branch].
	//				node2 parents asArray do: 
	//						[:node2Parent | 
	//						| newNode2 |
	//						node2Parent ~~ self 
	//							ifTrue: 
	//								[newNode2 := node2 duplicateFor: node2Parent branch.
	//								node2Parent replaceChild: node2 with: newNode2]].
	//				myNewNode2 := node2 duplicateFor: self branch.
	//				self replaceChild: node2 with: myNewNode2.
	//				^self singleRotateRightFor: matchingBranch].
	//		node2 parents asArray do: 
	//				[:node2Parent | 
	//				| newNode2 |
	//				node2Parent ~~ self 
	//					ifTrue: 
	//						[newNode2 := node2 duplicateFor: node2Parent branch.
	//						node2Parent replaceChild: node2 with: newNode2]].
	//		"parent := self singleParent: matchingRevision."
	//		"get existing dsp's"
	//		"a := parent dspForChild: self."
	//		b := self leftDsp.
	//		c := self rightDsp.
	//		d := self right leftDsp.
	//		e := self right rightDsp.
	//		"rotate right child (node2) to become  parent of self"
	//		self right: node2 left.
	//		"parent replaceChild: self with: node2."
	//		"update all dsps"
	//		self parents do: 
	//				[:parent | 
	//				| a |
	//				a := parent dspForChild: self.
	//				parent replaceChild: self with: node2.
	//				parent dsp: a + c forChild: node2].
	//		node2 left: self.
	//		node2 leftDsp: c negated.
	//		node2 rightDsp: e.
	//		node2 left leftDsp: b.
	//		node2 left rightDsp: c + d.
	//		^node2!
	//
	//	split
	//		^split!

	public int getSplit() {
		return split;
	}

	//
	//	split: anObject
	//		split := anObject!
	//
	//	transclusionsFrom: position extent: extent found: transclusions
	//		position < self split ifTrue: [self left transclusionsFrom: (self applyDsp: self leftDsp to: position) extent: extent found: transclusions].
	//		position + extent > self split ifTrue: [self right transclusionsFrom: (self applyDsp: self rightDsp to: position) extent: extent found: transclusions].
	//	! !
	//	!SplitNode categoriesFor: #applyDsp:to:!private! !
	//	!SplitNode categoriesFor: #assertIsChild:!private! !
	//	!SplitNode categoriesFor: #basicParentSplit!accessing!private! !
	//	!SplitNode categoriesFor: #basicSplayFor:!private! !
	//	!SplitNode categoriesFor: #childFor:do:!private! !
	//	!SplitNode categoriesFor: #children!public! !
	//	!SplitNode categoriesFor: #contents!public! !
	//	!SplitNode categoriesFor: #contentsAt:!public! !
	//	!SplitNode categoriesFor: #contentsFrom:extent:do:!accessing!public! !
	//	!SplitNode categoriesFor: #contentsFrom:extent:into:!accessing!public! !
	//	!SplitNode categoriesFor: #count!accessing!public! !
	//	!SplitNode categoriesFor: #dsp:forChild:!private! !
	//	!SplitNode categoriesFor: #dspForChild:!private! !
	//	!SplitNode categoriesFor: #duplicateFor:!private! !
	//	!SplitNode categoriesFor: #firstNodeFrom:extent:shouldSplit:!public! !
	//	!SplitNode categoriesFor: #insert:at:root:!public! !
	//	!SplitNode categoriesFor: #invertDsp:!private! !
	//	!SplitNode categoriesFor: #isLeft:!public! !
	//	!SplitNode categoriesFor: #left!accessing!private! !
	//	!SplitNode categoriesFor: #left:!accessing!private! !
	//	!SplitNode categoriesFor: #leftDsp!accessing!private! !
	//	!SplitNode categoriesFor: #leftDsp:!accessing!private! !
	//	!SplitNode categoriesFor: #nodeAt:!public! !
	//	!SplitNode categoriesFor: #removeChild:branch:!public! !
	//	!SplitNode categoriesFor: #removeChildLeft!private! !
	//	!SplitNode categoriesFor: #removeChildLeftFromRoot!private! !
	//	!SplitNode categoriesFor: #removeChildRightMaxElement!private! !
	//	!SplitNode categoriesFor: #replaceChild:with:!public! !
	//	!SplitNode categoriesFor: #right!accessing!private! !
	//	!SplitNode categoriesFor: #right:!accessing!private! !
	//	!SplitNode categoriesFor: #rightDsp!accessing!private! !
	//	!SplitNode categoriesFor: #rightDsp:!accessing!private! !
	//	!SplitNode categoriesFor: #singleRotateLeftFor:!private! !
	//	!SplitNode categoriesFor: #singleRotateRightFor:!private! !
	//	!SplitNode categoriesFor: #split!accessing!private! !
	//	!SplitNode categoriesFor: #split:!accessing!private! !
	//	!SplitNode categoriesFor: #transclusionsFrom:extent:found:!accessing!public! !
	//
	//
	//
}
