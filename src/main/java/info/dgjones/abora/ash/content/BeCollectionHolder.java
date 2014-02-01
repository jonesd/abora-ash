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

/**
 */
public class BeCollectionHolder extends BeContentElement {

	private final byte[] collection;

//	"Filed out from Dolphin Smalltalk 2002 release 5.00"!
//
//	BeContentElement subclass: #BeCollectionHolder
//		instanceVariableNames: 'collection'
//		classVariableNames: ''
//		poolDictionaries: ''
//		classInstanceVariableNames: ''!
//	BeCollectionHolder guid: (GUID fromString: '{46785AAF-450D-4C62-8737-3DFC64C37622}')!
//	BeCollectionHolder comment: ''!
//	!BeCollectionHolder categoriesForClass!Kernel-Objects! !
//	!BeCollectionHolder methodsFor!
//
//	collection
//		^collection!
//
//	collection: setCollection
//		self ensureValidCollection: setCollection.
//
//		collection := setCollection.!
//

	public int count() {
		return collection.length;
	}

//	createNodeAt: position for: forRevision
//		^CollectionLeaf 
//			branch: forRevision
//			startPosition: position
//			collection: self!
//
//	ensureValidCollection: potentialCollection
//		#todo "ensure that contents is either all integer or all float".
//		((potentialCollection isKindOf: Array) and: [potentialCollection allSatisfy: [:a | a isInteger]]) ifTrue: [^self].
//
//		Error signal: 'invalid element to insert into ent'.
//	!
//
//	printOn: aStream
//		self basicPrintOn: aStream.
//		aStream nextPutAll: '('.
//		self collection displayOn: aStream.
//		aStream nextPutAll: ')'! !
//	!BeCollectionHolder categoriesFor: #collection!accessing!private! !
//	!BeCollectionHolder categoriesFor: #collection:!accessing!private! !
//	!BeCollectionHolder categoriesFor: #createNodeAt:for:!public! !
//	!BeCollectionHolder categoriesFor: #ensureValidCollection:!accessing!private! !
//	!BeCollectionHolder categoriesFor: #printOn:!public! !
//
//	!BeCollectionHolder class methodsFor!
//

	public BeCollectionHolder(byte[] collection) {
		super();
		this.collection = collection;
	}

	public byte[] getCollection() {
		return collection;
	}

//	collection: array
//		^(self new)
//			collection: array;
//			yourself! !
//	!BeCollectionHolder class categoriesFor: #collection:!public! !
//

}
