/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.ent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.abora.ash.engine.AboraObject;

/**
 */
public abstract class EntNode extends AboraObject {
	protected SequenceNumber branch;

	public EntNode(SequenceNumber branch) {
		this.branch = branch;
	}

	protected abstract void addToParent(EntNode node);

	public List allEditions() {
		List roots = allRoots();
		List editions = new ArrayList(roots.size());
		for (Iterator iter = roots.iterator(); iter.hasNext();) {
			RootNode root = (RootNode) iter.next();
			editions.add(root.getEdition());
		}
		return editions;
	}

//	allEditions
//		^self allRoots collect: [:root | root edition]!
//

	public List allRoots() {
		List roots = new ArrayList();
		allRoots(roots);
		return roots;
	}

//	allRoots
//		| roots |
//		roots := IdentitySet new.
//		self allRoots: roots.
//		^roots!
//

	protected void allRoots(List allRoots) {
		for (Iterator iter = getParents().iterator(); iter.hasNext();) {
			EntNode node = (EntNode) iter.next();
			node.allRoots(allRoots);
		}
	}

//	allRoots: allRoots
//		self parents do: [:parent | parent allRoots: allRoots]!
//

	protected SplitNode basicParentSplit() {
		return null;
	}

//	basicParentSplit
//		^nil!
//

	protected abstract EntNode basicSplayFor(SequenceNumber matchingBranch);

	public SequenceNumber getBranch() {
		return branch;
	}

//	branch
//		^branch!
//
//	branch: anObject
//		branch := anObject!
//

	public abstract List children();

//	children
//		self subclassResponsibility!
//
//	contents
//		self subclassResponsibility!
//
//	contentsAt: position
//		self subclassResponsibility!
//
//	contentsFrom: position extent: extent
//		| contents |
//		#todo "max:".
//	extent < 0 ifTrue: [self halt].
//		contents := WriteStream on: (Array new: (extent max: 0)).
//		self contentsFrom: position extent: extent into: contents.
//	"	self contentsFrom: position extent: extent do: [:char | contents nextPut: char]."
//		^contents contents!
//
//	contentsFrom: position extent: extent into: stream
//		self subclassResponsibility!
//
	public abstract int count();
	
//	count
//		self subclassResponsibility!
//

  //TODO shold this be in a ParentNode interface?
	protected abstract int dspForChild(EntNode node);
	
	protected abstract void setDspForChild(int dsp, EntNode child);

	public abstract EntNode duplicateFor(SequenceNumber newBranch) throws EntException;
	
//	duplicateFor: newBranch
//		self subclassResponsibility!
//
//	duplicateWithParentsFor: newBranch
//		| duplicate parent parentDuplicate |
//		duplicate := self duplicateFor: newBranch.
//		parent := self singleParentFor: newBranch.
//		parent notNil 
//			ifTrue: 
//				[parentDuplicate := parent duplicateWithParentsFor: newBranch.
//				parentDuplicate replaceChild: self with: duplicate].
//		^duplicate!
//
	protected void ensureCompatibleFor(SequenceNumber matchingBranch) throws NonCompatibleBranchException {
		if (!isCompatibleFor(matchingBranch)) {
			throw new NonCompatibleBranchException();
		}
	}

//	ensureCompatibleFor: matchingBranch
//		(self isCompatibleFor: matchingBranch) 
//			ifFalse: [EntError signal: 'not-compatible ent revision']!
//

	protected void ensureSameFor(SequenceNumber matchingBranch) throws NonSameBranchException {
		if (!isSameFor(matchingBranch)) {
			throw new NonSameBranchException();
		}
	}

//	ensureSameFor: matchingBranch
//		(self isSameFor: matchingBranch) ifFalse: [EntError signal: 'not-same ent revision']!
//

protected EntNode findBestParentMatchFor(SequenceNumber matchingBranch) {
	EntNode singleParent = null;
	for (Iterator i = getParents().iterator(); i.hasNext(); ) {
		EntNode possibleParent = (EntNode) i.next();
		if (possibleParent.isSameFor(matchingBranch)) {
			return possibleParent;
		}
		if (possibleParent.isCompatibleFor(matchingBranch) && (
			singleParent == null || !possibleParent.getBranch().isBranchingAfter(singleParent.getBranch()))) {
				singleParent = possibleParent;
			}
	}
	return singleParent;
}

//	findBestParentMatchFor: matchingBranch
//		| singleParent |
//		singleParent := nil.
//		self parents do: 
//				[:possibleParent | 
//				(possibleParent isSameFor: matchingBranch) ifTrue: [^possibleParent].
//				((possibleParent isCompatibleFor: matchingBranch) 
//					and: [singleParent isNil or: [possibleParent branch > singleParent branch]]) 
//						ifTrue: [singleParent := possibleParent]].
//		^singleParent!
//

  	public abstract void insertLeaf(LeafNode leaf, int position, RootNode root);

	protected boolean isCompatibleFor(SequenceNumber matchingBranch) {
		return branch.isBranchingBeforeOrEqual(matchingBranch);
	}
//	isCompatibleFor: matchingBranch
//		^self branch isBeforeOrEqual: matchingBranch!
//
	protected boolean isSameFor(SequenceNumber matchingBranch) {
		//@todo do we actually want something more specific - ie trailing zeros
		return branch.equals(matchingBranch);
	}

//	isSameFor: matchingBranch
//		^self branch = matchingBranch!
//

	protected EntNode maxNode() {
		List list = children();
		return (EntNode)list.get(list.size() - 1);
	}

//	maxNode
//		^self children last maxNode!
//

  protected EntNode minNode() {
	  return (EntNode)children().get(0);
  }

//	minNode
//		^self children first minNode!
//

	public abstract List getParents();

//	parents
//		self subclassResponsibility!
//

	protected SplitNode parentSplit(SequenceNumber matchingBranch) {
		EntNode parent = singleParentFor(matchingBranch);
		if (parent != null) {
			return parent.basicParentSplit();
		} else {
			return null;
		}
	}

//	parentSplit: matchingBranch
//		| parent |
//		parent := self singleParentFor: matchingBranch.
//		^parent notNil ifTrue: [parent basicParentSplit] ifFalse: [nil]!
//

	protected abstract void removeFromParent(EntNode oldParent);

  protected abstract void replaceChild(EntNode existingChild, EntNode newChild);

protected RootNode rootFor(SequenceNumber matchingBranch) throws NonCompatibleBranchException {
	EntNode parent = singleParentFor(matchingBranch);
	//TODO null parent might indicate somethingt gone wrong
	if (parent != null) {
		return parent.rootFor(matchingBranch);
	} else {
		return null;
	}
}

//	rootFor: matchingBranch
//		| parent |
//		parent := self singleParentFor: matchingBranch.
//		#todo.	"null parent might indicate something gong wrong"
//		^parent notNil ifTrue: [parent rootFor: matchingBranch] ifFalse: [nil]!
//
//	sharedWith: anotherEdition for: forBranch mappings: mappings
//		self children do: 
//				[:child | 
//				child 
//					sharedWith: anotherEdition
//					for: forBranch
//					mappings: mappings]!
//

protected EntNode singleParentFor(SequenceNumber matchingBranch) {
	if (getParents().isEmpty()) {
		return null;
	} else {
		EntNode singleParent = findBestParentMatchFor(matchingBranch);
		//TODO investigate the commenting out of the following
		//self assert: [singleParent notNil]
		return singleParent;
	}
}

//	singleParentFor: matchingBranch
//		^self parents isEmpty 
//			ifTrue: [nil]
//			ifFalse: 
//				[| singleParent |
//				singleParent := self findBestParentMatchFor: matchingBranch.
//				#todo.	"investigate the commenting out of the following"
//				"self assert: [singleParent notNil]."
//				singleParent]!
//
//	transclusions: transclusions
//		self children do: [:child | child transclusions: transclusions]! !
//	!EntNode categoriesFor: #allEditions!public! !
//	!EntNode categoriesFor: #allRoots!public! !
//	!EntNode categoriesFor: #allRoots:!private! !
//	!EntNode categoriesFor: #basicParentSplit!private! !
//	!EntNode categoriesFor: #branch!accessing!public! !
//	!EntNode categoriesFor: #branch:!accessing!private! !
//	!EntNode categoriesFor: #children!public! !
//	!EntNode categoriesFor: #contents!public! !
//	!EntNode categoriesFor: #contentsAt:!public! !
//	!EntNode categoriesFor: #contentsFrom:extent:!public! !
//	!EntNode categoriesFor: #contentsFrom:extent:into:!public! !
//	!EntNode categoriesFor: #count!public! !
//	!EntNode categoriesFor: #duplicateFor:!private! !
//	!EntNode categoriesFor: #duplicateWithParentsFor:!private! !
//	!EntNode categoriesFor: #ensureCompatibleFor:!private! !
//	!EntNode categoriesFor: #ensureSameFor:!private! !
//	!EntNode categoriesFor: #findBestParentMatchFor:!private! !
//	!EntNode categoriesFor: #isCompatibleFor:!private! !
//	!EntNode categoriesFor: #isSameFor:!private! !
//	!EntNode categoriesFor: #maxNode!public! !
//	!EntNode categoriesFor: #minNode!public! !
//	!EntNode categoriesFor: #parents!public! !
//	!EntNode categoriesFor: #parentSplit:!private! !
//	!EntNode categoriesFor: #rootFor:!private! !
//	!EntNode categoriesFor: #sharedWith:for:mappings:!public! !
//	!EntNode categoriesFor: #singleParentFor:!private! !
//	!EntNode categoriesFor: #transclusions:!public! !
//
//	!EntNode class methodsFor!
//
//	basicNew
//		^(super basicNew)
//			initialize;
//			yourself!
//
//	new
//		^self shouldNotImplement! !
//	!EntNode class categoriesFor: #basicNew!public! !
//	!EntNode class categoriesFor: #new!public! !
//
//
}
