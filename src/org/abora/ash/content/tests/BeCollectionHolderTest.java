/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */
package org.abora.ash.content.tests;

import junit.framework.TestCase;

import org.abora.ash.content.BeCollectionHolder;
import org.abora.ash.engine.AboraConverter;

/**
 */
public class BeCollectionHolderTest extends TestCase {

	//	"Filed out from Dolphin Smalltalk 2002 release 5.00"!
	//
	//	AboraBeTests subclass: #BeCollectionHolderTest
	//		instanceVariableNames: ''
	//		classVariableNames: ''
	//		poolDictionaries: ''
	//		classInstanceVariableNames: ''!
	//	BeCollectionHolderTest guid: (GUID fromString: '{057A8346-027D-402E-B62F-145355E68B95}')!
	//	BeCollectionHolderTest comment: ''!
	//	!BeCollectionHolderTest categoriesForClass!Kernel-Objects! !
	//	!BeCollectionHolderTest methodsFor!
	//
	public void testCount() {
		BeCollectionHolder holder = new BeCollectionHolder(AboraConverter.toAboraContent("hello"));
		assertEquals(5, holder.count());
	}

	public void testCreate() {
		BeCollectionHolder holder = new BeCollectionHolder(AboraConverter.toAboraContent("hello"));
		assertEquals("hello", AboraConverter.toJavaString(holder.getCollection()));
	}

	//	testCreate
	//		| content |
	//		content := BeCollectionHolder collection: 'hello' asAboraContent.
	//		self should: [content collection = 'hello' asAboraContent]!
	//
	//	testCreateBadCollection
	//		self should: [BeCollectionHolder collection: 'hello'] raise: Error.
	//		self should: [BeCollectionHolder collection: nil] raise: Error.
	//		self should: [BeCollectionHolder collection: #(1 1.0)] raise: Error.
	//		self should: [BeCollectionHolder collection: BeEdition new] raise: Error!
	//
	//	testCreateNodeAtFor
	//		| content node |
	//		content := BeCollectionHolder collection: 'ABC' asAboraContent.
	//		node := content createNodeAt: 1 for: 2 asBranchingNumber.
	//		self should: [node contents = 'ABC' asAboraContent].
	//		self should: [node branch  = 2 asBranchingNumber].
	//		self should: [node startPosition = 1].!
	//
	//	testPrintOn
	//		| content |
	//		content := BeCollectionHolder collection: 'ABC' asAboraContent.
	//		self should: [content printString = 'a BeCollectionHolder(#(65 66 67))']! !
	//	!BeCollectionHolderTest categoriesFor: #testCreate!public! !
	//	!BeCollectionHolderTest categoriesFor: #testCreateBadCollection!public! !
	//	!BeCollectionHolderTest categoriesFor: #testCreateNodeAtFor!public! !
	//	!BeCollectionHolderTest categoriesFor: #testPrintOn!public! !
	//

}
