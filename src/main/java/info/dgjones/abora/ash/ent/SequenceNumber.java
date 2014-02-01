/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package info.dgjones.abora.ash.ent;

import java.util.Iterator;

/**
 */
public class SequenceNumber implements Comparable {
	private final int digits[];

	public SequenceNumber(int digit1) {
		digits = new int[] {digit1};
	}
	public SequenceNumber(int digit1, int digit2) {
		digits = new int[] {digit1, digit2};
	}
	public SequenceNumber(int digit1, int digit2, int digit3) {
		digits = new int[] {digit1, digit2, digit3};
	}
	public SequenceNumber(int[] uncapturedDigits) {
		//@todo support empty digits?
		digits = new int[uncapturedDigits.length];
		for (int  i = 0;  i < uncapturedDigits.length;  i++) {
			int digit = uncapturedDigits[i];
			digits[i] = digit;			
		}
	}


	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Object o) {
		return compareTo((SequenceNumber) o);
	}
	public int compareTo(SequenceNumber n) {
		if (this == n)
			return 0;
		for (DigitInterleavingIterator i = new DigitInterleavingIterator(digits, n.digits); i.hasNext();) {
			int digit1 = i.nextInt();
			int digit2 = i.nextInt();
			if (digit1 < digit2)
				return -1;
			else if (digit1 > digit2)
				return 1;
		}
		return 0;		
	}
	public String toString() {
		StringBuffer buffer = new StringBuffer("SequenceNumber[");
		for (int i = 0; i < digits.length; i++) {
			if (i > 0)
				buffer.append(":");
			buffer.append(digits[i]);
		}
		buffer.append("]");
		return buffer.toString();
	}
	protected class DigitInterleavingIterator implements Iterator {
		private final int[] digits1;
		private final int[] digits2;
		private int nextDigits1 = 0;
		private int nextDigits2 = 0;
		public DigitInterleavingIterator(int[] digits1, int[] digits2) {
			this.digits1 = digits1;
			this.digits2 = digits2;
		}
		public boolean hasNext() {
			return nextDigits1 < digits1.length || nextDigits2 < digits2.length;
		}
		public Object next() {
			return new Integer(nextInt());
		}
		public int nextInt() {
//			if (!hasNext())
//				throw new NoSuchElementException();
			int value = 0;
			if (nextDigits1 <= nextDigits2) {
				if (nextDigits1 < digits1.length) {
					value = digits1[nextDigits1];
				}
				nextDigits1++;
			} else {
				if (nextDigits2 < digits2.length) {
					value = digits2[nextDigits2];
				}
				nextDigits2++;
			}
			return value;
		}
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

	public SequenceNumber copyWith(int newDigit) {
		int[] newDigits = new int[digits.length + 1];
		for (int i = 0; i < digits.length; i++) {
			int digit = digits[i];
			newDigits[i] = digit;
		}
		newDigits[newDigits.length - 1] = newDigit;
		return new SequenceNumber(newDigits);
	}
	public boolean equals(Object o) {
		if (!(o instanceof SequenceNumber))
			return false;
		return compareTo(o) == 0;
	}
	public int hashCode() {
		int value = 0;
		for (int i = 0; i < digits.length; i++) {
			int digit = digits[i];
			if (digit != 0) {
				value = value << 1 | digit;
			}
		}
		return value;
	}
	public SequenceNumber incrementLast() {
		// @todo what if there are now digits?
		int[] newDigits = new int[digits.length];
		for (int i = 0; i < digits.length; i++) {
			int digit = digits[i];
			if (i == digits.length - 1) {
				digit++;
			}
			newDigits[i] = digit;
		}
		return new SequenceNumber(newDigits);
	}

//	subtractFromInteger: anInteger
//		"Private - Answer the result of subtracting the receiver from the known integer,
//		 anInteger, by coercing the less general of it and the receiver. Overridden by 
//		subclasses which can implement more efficiently."
//
//		| newDigits |
//		newDigits := OrderedCollection new.
//		anInteger asSequenceMagnitude digitsWith: self do: [:digit1 :digit2 | newDigits add: digit1 - digit2].
//		^self class withAll: newDigits asArray.
//	!

	public boolean isBranchingAfter(SequenceNumber n) {
		return n.isBranchingBefore(this);
	}

	public boolean isBranchingBefore(SequenceNumber n) {
		if (digits.length > n.digits.length)
			return false;
		for (int i = 0; i < digits.length - 1; i++) {
			int digit1 = digits[i];
			int digit2 = n.digits[i];
			if (digit1 != digit2)
				return false;
		}
		if (digits.length < n.digits.length) {
			return digits[digits.length - 1] <= n.digits[digits.length - 1];
		} else {
			return digits[digits.length - 1] < n.digits[digits.length - 1];
		}
	}
	
	public boolean isBranchingBeforeOrEqual(SequenceNumber n) {
		return isBranchingBefore(n) || equals(n);
	}

	public int[] toIntArray() {
		int[] newDigits = new int[digits.length];
		for (int i = 0; i < digits.length; i++) {
			int digit = digits[i];
			newDigits[i] = digit;
		}
		return newDigits;
	}
}
