/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.content;

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
