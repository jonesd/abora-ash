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
public class BeDataHolder extends BeContentElement {
	
	private final BeContentElement value;
	
	public BeDataHolder(BeContentElement value) {
		super();
		this.value = value;
	}
	
	public BeContentElement getValue() {
		return value;
	}
	
	
	
//	"Filed out from Dolphin Smalltalk 2002 release 5.00"!
//
//	BeContentElement subclass: #BeDataHolder
//		instanceVariableNames: 'value'
//		classVariableNames: ''
//		poolDictionaries: ''
//		classInstanceVariableNames: ''!
//	BeDataHolder guid: (GUID fromString: '{E7A180D5-EAC0-4DEB-B9E0-6ACDFA73FB06}')!
//	BeDataHolder comment: ''!
//	!BeDataHolder categoriesForClass!Kernel-Objects! !
//	!BeDataHolder methodsFor!
//
//	printOn: aStream
//		self basicPrintOn: aStream.
//		aStream nextPutAll: '('.
//		self value displayOn: aStream.
//		aStream nextPutAll: ')'!
//
//	value
//		^value!
//
//	value: anObject
//		value := anObject! !
//	!BeDataHolder categoriesFor: #printOn:!public! !
//	!BeDataHolder categoriesFor: #value!accessing!private! !
//	!BeDataHolder categoriesFor: #value:!accessing!private! !
//
//	!BeDataHolder class methodsFor!
//
//	value: dataValue
//		^(self new)
//			value: dataValue;
//			yourself! !
//	!BeDataHolder class categoriesFor: #value:!public! !
//

}
