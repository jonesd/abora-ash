/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.ash.content;

import java.util.HashSet;
import java.util.Set;

import info.dgjones.abora.ash.engine.AboraObject;
import info.dgjones.abora.ash.ent.LeafNode;

/**
 */
public class BeContentElement extends AboraObject {

	protected Set parents;
	
	protected BeContentElement() {
		parents = new HashSet();
	}

//	"Filed out from Dolphin Smalltalk 2002 release 5.00"!
//
//	AboraObject subclass: #BeContentElement
//		instanceVariableNames: 'parents'
//		classVariableNames: ''
//		poolDictionaries: ''
//		classInstanceVariableNames: ''!
//	BeContentElement guid: (GUID fromString: '{29FCADDE-72E1-4551-88EA-D8C71B0A338A}')!
//	BeContentElement comment: ''!
//	!BeContentElement categoriesForClass!Kernel-Objects! !
//	!BeContentElement methodsFor!
//

	public void addParent(LeafNode node) {
		parents.add(node);
	}

//	addParent: parent
//		parents add: parent!
//
//	createNodeAt: position for: forRevision
//		^ContentLeaf 
//			branch: forRevision
//			startPosition: position
//			contentElement: self!
//
//	filterTransclusions: editions by: filter
//		^editions select: [:edition | filter allSatisfy: [:requiredEdition | edition endorsements includes: requiredEdition]].!
//
//	initialize
//		super initialize.
//
//		parents := OrderedCollection new.!
//
//	makeFeProxy
//		self subclassResponsibility!
//

	public Set getParents() {
		return parents;
	}

//	parents
//		^parents!
//

	public void removeParent(LeafNode parent) {
		parents.remove(parent);
	}

//	removeParent: parent
//		parents remove: parent!
//
//	stbSaveOn: anSTBOutFiler
//		(anSTBOutFiler context isKindOf: BeSession) 
//			ifTrue: 
//				[anSTBOutFiler context addFeContentElementFor: self.
//				anSTBOutFiler saveObject: self as: (STBBeContentElementForFE for: self)]
//			ifFalse: 
//				[self assert: [anSTBOutFiler context ~~ #forBe].
//				"self halt."
//				super stbSaveOn: anSTBOutFiler]!
//
//	transclusionsDirect
//		"return an edition"
//
//		| editions |
//		#todo.
//		editions := IdentitySet new.
//		self parents do: [:leaf | editions addAll: leaf allEditions].
//		editions remove: self
//			ifAbsent: 
//				["ignore"
//
//				].
//		^editions!
//
//	transclusionsDirectFilteredBy: filter
//		| editions |
//		editions := self transclusionsDirect.
//		^self filterTransclusions: editions by: filter
//	! !
//	!BeContentElement categoriesFor: #addParent:!must not strip!public! !
//	!BeContentElement categoriesFor: #createNodeAt:for:!must not strip!public! !
//	!BeContentElement categoriesFor: #filterTransclusions:by:!must not strip!public! !
//	!BeContentElement categoriesFor: #initialize!private! !
//	!BeContentElement categoriesFor: #makeFeProxy!private! !
//	!BeContentElement categoriesFor: #parents!must not strip!public! !
//	!BeContentElement categoriesFor: #removeParent:!must not strip!public! !
//	!BeContentElement categoriesFor: #stbSaveOn:!private! !
//	!BeContentElement categoriesFor: #transclusionsDirect!must not strip!public! !
//	!BeContentElement categoriesFor: #transclusionsDirectFilteredBy:!must not strip!public! !
//

}
