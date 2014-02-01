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

import junit.framework.TestCase;

import info.dgjones.abora.ash.content.BeCollectionHolder;
import info.dgjones.abora.ash.engine.AboraConverter;

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
