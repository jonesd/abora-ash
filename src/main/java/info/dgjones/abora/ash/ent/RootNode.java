/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package info.dgjones.abora.ash.ent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import info.dgjones.abora.ash.content.BeEdition;

/**
 */
public class RootNode extends EntNode {
	private EntNode child = null;
	private int dsp = 0;
	private final BeEdition edition;

	public RootNode(BeEdition edition, SequenceNumber branch) {
		super(branch);
		this.edition = edition;
	}
	public RootNode(BeEdition edition, SequenceNumber branch, ChildNode child) {
		this(edition, branch, child, 0);
	}
	public RootNode(BeEdition edition, SequenceNumber branch, ChildNode child, int dsp) {
		super(branch);
		this.edition = edition;
		this.dsp = dsp;
		setChild(child);
	}


	protected void allRoots(List allRoots) {
		allRoots.add(this);
	}

//	allRoots: allRoots
//		allRoots add: self!
//

	protected int applyDspTo(int position) {
		return position + invertDsp();
	}

//	applyDspTo: position
//		| dspPosition |
//		dspPosition := position + (self invertDsp).
//		^dspPosition!
//

protected void assertIsChild(EntNode node) {
	if (node != child) {
		throw new IllegalStateException("unknown child");
	}
}

//	assertIsChild: node
//		self child ~~ node ifTrue: [EntError signal: 'unknown child of root']!
//

	protected EntNode basicSplayFor(SequenceNumber matchingBranch) {
		//TODO only called in the case of a single child
		ensureSameFor(matchingBranch);
		return getChild();
	}

//	basicSplayFor: matchingBranch
//		"only called in the case of a single child"
//
//		#todo.
//		self ensureSameFor: matchingBranch.
//		^self child!
//

	public EntNode getChild() {
		return child;
	}
//	child
//		^child!
//

	protected void setChild(EntNode node) {
		if (child == node) {
			return;
		}
		if (child != null) {
			child.removeFromParent(this);
		}
		child = node;
		//TODO reset dsp if nil?
		if (node != null) {
			node.addToParent(this);
		}
	}

//	child: aNodeOrNil
//		child == aNodeOrNil ifTrue: [^self].
//		child notNil ifTrue: [child removeFromParent: self].
//		child := aNodeOrNil.
//		#todo.	"reset dsp if nil?"
//		aNodeOrNil notNil ifTrue: [aNodeOrNil addToParent: self]!
//
	public List children() {
		if (!hasChild())
			return Collections.EMPTY_LIST;
		List children = new ArrayList(1);
		children.add(child);
		return children;
	}

//	children
//		^self child notNil ifTrue: [Array with: self child] ifFalse: [#()]!
//
//	contents
//		^self contentsFrom: 1 extent: self count!
//
//	contentsAt: position
//		child notNil 
//			ifTrue: [^self child contentsAt: (self applyDspTo: position)]
//			ifFalse: [self errorSubscriptBounds: position]!
//
//	contentsFrom: position extent: extent do: operation
//		^self child notNil 
//			ifTrue: 
//				[self child 
//					contentsFrom: (self applyDspTo: position)
//					extent: extent
//					do: operation]
//			ifFalse: ['']!
//
//	contentsFrom: position extent: extent into: stream
//		self child notNil 
//			ifTrue: 
//				[self child 
//					contentsFrom: (self applyDspTo: position)
//					extent: extent
//					into: stream]
//			ifFalse: []!

	public int count() {
		return hasChild() ? child.count() : 0;
	}
//
//	count
//		^child notNil ifTrue: [child count] ifFalse: [0]!
//

	public int getDsp() {
		return dsp;
	}
	
	public void setDsp(int dsp) {
		this.dsp = dsp;
	}
//	dsp
//		^dsp!
//
//	dsp: anObject
//		dsp := anObject!
//

protected void setDspForChild(int dsp, EntNode node) {
	assertIsChild(node);
	this.dsp = dsp;
}

//	dsp: newDsp forChild: node
//		self assertIsChild: node.
//		self dsp: newDsp!
//

protected int dspForChild(EntNode child) {
	assertIsChild(child);
	return dsp;
}

//	dspForChild: node
//		self assertIsChild: node.
//		^self dsp!
//
	public EntNode duplicateFor(SequenceNumber newBranch) {
		// Do not duplicate RootNodes at the moment, as this forced to happen on newEdition.
		// @todo is this the right behaviour - not duplicating?
		ensureSameFor(newBranch);
		return this;		
	}

//	duplicateFor: newBranch
//		"Do not duplicate RootNodes at the moment, as this forced to happen on newEdition."
//
//		"is this the right behaviour - not duplicating?"
//
//		#todo.
//		self ensureSameFor: newBranch.
//		^self!
//

	public BeEdition getEdition() {
		return edition;
	}
//	edition
//		^edition!
//
//	edition: anObject
//		edition := anObject!
//
//	firstNodeFrom: position extent: extent shouldSplit: shouldSplit
//		child notNil 
//			ifTrue: 
//				[^self child 
//					firstNodeFrom: (self applyDspTo: position)
//					extent: extent
//					shouldSplit: shouldSplit]
//			ifFalse: [self errorSubscriptBounds: position]!
//

	public boolean hasChild() {
		return child != null;
	}
//	hasChild
//		^child notNil!
//
//	initialize
//		super initialize.
//
//		self dsp: 0.!
//


  public void insert(LeafNode leaf) {
  	//TODO what abut dsp?
	  ensureSameFor(leaf.getBranch());
	  if (hasChild()) {
	  	child.insertLeaf(leaf, applyDspTo(leaf.getStartPosition()), this);
	  } else {
	  	//TODO review this default location of 1
	  	if (leaf.getStartPosition() == 0) {
	  		setChild(leaf);
	  	} else {
	  		throw new IndexOutOfBoundsException(String.valueOf(leaf.getStartPosition()));
	  	}
	  } 
  }

	public void insertLeaf(LeafNode leaf, int position, RootNode root) {
		throw new UnsupportedOperationException();
	}

//	insert: dataLeaf
//		"what about dsp?"
//
//		#todo.
//		self ensureSameFor: dataLeaf branch.
//		child isNil 
//			ifTrue: 
//				[dataLeaf startPosition = 1 
//					ifTrue: 
//						[child := dataLeaf.
//						dataLeaf addToParent: self]
//					ifFalse: [self errorSubscriptBounds: dataLeaf startPosition]]
//			ifFalse: 
//				[self child 
//					insert: dataLeaf
//					at: (self applyDspTo: dataLeaf startPosition)
//					root: self]!
//

	protected int invertDsp() {
		return -dsp;
	}

//	invertDsp
//		^dsp negated!
//
//	nodeAt: position
//		child notNil 
//			ifTrue: [^self child nodeAt: (self applyDspTo: position)]
//			ifFalse: [self errorSubscriptBounds: position]!
//
//	nodesFrom: startPosition extent: extent shouldSplit: shouldSplit
//		| position nodes extentLeft |
//		nodes := OrderedCollection new.
//		position := startPosition.
//		extentLeft := extent.
//		[extentLeft > 0] whileTrue: 
//				[| node |
//				node := self 
//							firstNodeFrom: position
//							extent: extentLeft
//							shouldSplit: shouldSplit.
//				nodes add: node.
//				position := position + node count.
//				#todo "extentLeft is bogus if shouldSplit = false".
//				extentLeft := extentLeft - node count].
//		^nodes!
//
	public List getParents() {
		return Collections.EMPTY_LIST;
	}

//	parents
//		^#()!
//
//	removeChild: existingChild branch: newBranch
//		self ensureSameFor: newBranch.
//		self replaceChild: existingChild with: nil.
//		self dsp: 0!
//

  protected void replaceChild(EntNode existingChild, EntNode newChild) {
  	assertIsChild(existingChild);
  	setChild(newChild);
  }


//	replaceChild: existingChild with: newChild
//		self assertIsChild: existingChild.
//		self child: newChild.!
//

	public RootNode rootFor(SequenceNumber matchingBranch) throws NonCompatibleBranchException {
		ensureCompatibleFor(matchingBranch);
		return this;
	}

	protected void removeFromParent(EntNode oldParent) {
		throw new UnsupportedOperationException("no parent");
	}

	protected void addToParent(EntNode node) {
		throw new UnsupportedOperationException("no parent");
	}

//	rootFor: matchingBranch
//		self ensureCompatibleFor: matchingBranch.
//		^self!
//
//	splay: position
//		| node newChild |
//		node := self nodeAt: position.
//		newChild := node basicSplayFor: self branch.
//		self child: newChild.
//		^node!
//
//	transclusionsFrom: start extent: extent found: transclusions
//		self hasChild ifTrue: [self child transclusionsFrom: (self applyDspTo: start) extent: extent found: transclusions].! !
//	!RootNode categoriesFor: #allRoots:!private! !
//	!RootNode categoriesFor: #applyDspTo:!private! !
//	!RootNode categoriesFor: #assertIsChild:!private! !
//	!RootNode categoriesFor: #basicSplayFor:!private! !
//	!RootNode categoriesFor: #child!private! !
//	!RootNode categoriesFor: #child:!accessing!private! !
//	!RootNode categoriesFor: #children!public! !
//	!RootNode categoriesFor: #contents!public! !
//	!RootNode categoriesFor: #contentsAt:!public! !
//	!RootNode categoriesFor: #contentsFrom:extent:do:!public! !
//	!RootNode categoriesFor: #contentsFrom:extent:into:!public! !
//	!RootNode categoriesFor: #count!public! !
//	!RootNode categoriesFor: #dsp!accessing!private! !
//	!RootNode categoriesFor: #dsp:!accessing!private! !
//	!RootNode categoriesFor: #dsp:forChild:!public! !
//	!RootNode categoriesFor: #dspForChild:!public! !
//	!RootNode categoriesFor: #duplicateFor:!public! !
//	!RootNode categoriesFor: #edition!accessing!public! !
//	!RootNode categoriesFor: #edition:!accessing!private! !
//	!RootNode categoriesFor: #firstNodeFrom:extent:shouldSplit:!public! !
//	!RootNode categoriesFor: #hasChild!public! !
//	!RootNode categoriesFor: #initialize!private! !
//	!RootNode categoriesFor: #insert:!public! !
//	!RootNode categoriesFor: #invertDsp!private! !
//	!RootNode categoriesFor: #nodeAt:!public! !
//	!RootNode categoriesFor: #nodesFrom:extent:shouldSplit:!public! !
//	!RootNode categoriesFor: #parents!public! !
//	!RootNode categoriesFor: #removeChild:branch:!private! !
//	!RootNode categoriesFor: #replaceChild:with:!private! !
//	!RootNode categoriesFor: #rootFor:!public! !
//	!RootNode categoriesFor: #splay:!public! !
//	!RootNode categoriesFor: #transclusionsFrom:extent:found:!public! !
//
//
}
