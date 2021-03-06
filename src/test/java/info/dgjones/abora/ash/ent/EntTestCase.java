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
package info.dgjones.abora.ash.ent;

import junit.framework.TestCase;

import info.dgjones.abora.ash.ent.EntNode;

/**
 */
public abstract class EntTestCase extends TestCase {

	public EntTestCase(String name) {
		super(name);
	}

//	"Filed out from Dolphin Smalltalk 2002 release 5.00"!
//
//	AboraTests subclass: #AboraBeTests
//		instanceVariableNames: ''
//		classVariableNames: ''
//		poolDictionaries: ''
//		classInstanceVariableNames: ''!
//	AboraBeTests guid: (GUID fromString: '{8B40BB18-EAF8-4BB9-8BBD-4644045F1CBE}')!
//	AboraBeTests comment: ''!
//	!AboraBeTests categoriesForClass!Kernel-Objects! !
//	!AboraBeTests methodsFor!
//

	protected void assertTextContents(String expectedText, EntNode node) {
		//TODO
	}

//	assertTextContentsOf: root is: expectedText
//		| actualText |
//		actualText := root contents asAboraText.
//		self should: [actualText = expectedText].
//		expectedText
//			keysAndValuesDo: [:i :char | self should: [(root contentsAt: i) = char codePoint]]!
//
//	createBalanced12345678
//		| splitC1 splitC2 splitC3 splitA splitC4 splitB1 splitB2 |
//		splitC1 := SplitNode 
//					branch: 1
//					split: 2
//					left: (CollectionLeaf 
//							branch: 1
//							startPosition: 1
//							elements: '1' asAboraContent)
//					right: (CollectionLeaf 
//							branch: 1
//							startPosition: 2
//							elements: '2' asAboraContent).
//		splitC2 := SplitNode 
//					branch: 1
//					split: 4
//					left: (CollectionLeaf 
//							branch: 1
//							startPosition: 3
//							elements: '3' asAboraContent)
//					right: (CollectionLeaf 
//							branch: 1
//							startPosition: 4
//							elements: '4' asAboraContent).
//		splitC3 := SplitNode 
//					branch: 1
//					split: 6
//					left: (CollectionLeaf 
//							branch: 1
//							startPosition: 5
//							elements: '5' asAboraContent)
//					right: (CollectionLeaf 
//							branch: 1
//							startPosition: 6
//							elements: '6' asAboraContent).
//		splitC4 := SplitNode 
//					branch: 1
//					split: 8
//					left: (CollectionLeaf 
//							branch: 1
//							startPosition: 7
//							elements: '7' asAboraContent)
//					right: (CollectionLeaf 
//							branch: 1
//							startPosition: 8
//							elements: '8' asAboraContent).
//		splitB1 := SplitNode 
//					branch: 1
//					split: 3
//					left: splitC1
//					right: splitC2.
//		splitB2 := SplitNode 
//					branch: 1
//					split: 7
//					left: splitC3
//					right: splitC4.
//		splitA := SplitNode 
//					branch: 1
//					split: 5
//					left: splitB1
//					right: splitB2.
//		^RootNode 
//			edition: nil
//			branch: 1
//			with: splitA!
//
//	createSourceEdition12345678
//		| edition |
//		edition := BeEdition new.
//		edition root insert: (CollectionLeaf 
//					branch: 1
//					startPosition: 1
//					elements: '12345678' asAboraContent).
//		^edition! !
//	!AboraBeTests categoriesFor: #assertTextContentsOf:is:!public! !
//	!AboraBeTests categoriesFor: #createBalanced12345678!private! !
//	!AboraBeTests categoriesFor: #createSourceEdition12345678!private! !
//
//	!AboraBeTests class methodsFor!
//
//	isAbstract
//		"Override to true if a TestCase subclass is Abstract and should not have
//		TestCase instances built from it"
//
//		^self name = #AboraBeTests! !
//	!AboraBeTests class categoriesFor: #isAbstract!public! !
//

}
