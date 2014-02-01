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

import info.dgjones.abora.ash.ent.SequenceNumber;

import junit.framework.TestCase;

/**
 */
public class SequenceNumberTest extends TestCase {

	public SequenceNumberTest(String arg0) {
		super(arg0);
	}
	private void assertEquals(int[] a, int[] b) {
		assertEquals("size", a.length, b.length);
		for (int i = 0; i < a.length; i++) {
			assertEquals("i"+i, a[i], b[i]);
		}
	}

	public void testCopyWith() {
		assertEquals(new SequenceNumber(1).copyWith(0).toIntArray(), new int[]{1,0});
		assertEquals(new SequenceNumber(3).copyWith(1).toIntArray(), new int[]{3,1});
		assertEquals(new SequenceNumber(1, 2).copyWith(3).toIntArray(), new int[]{1,2,3});
	}
	public void testNew() {
		assertEquals(new SequenceNumber(1).toIntArray(), new int[]{1});
		assertEquals(new SequenceNumber(1,2).toIntArray(), new int[]{1,2});
		assertEquals(new SequenceNumber(1,2,3).toIntArray(), new int[]{1,2,3});
		assertEquals(new SequenceNumber(new int[]{1,2,3}).toIntArray(), new int[]{1,2,3});
	}
	public void testToString() {
		assertEquals(new SequenceNumber(1).toString(), "SequenceNumber[1]");
		assertEquals(new SequenceNumber(1,2).toString(), "SequenceNumber[1:2]");
		assertEquals(new SequenceNumber(1,2,3).toString(), "SequenceNumber[1:2:3]");
	}
	public void testEquals() {
		assertEquals(new SequenceNumber(1), new SequenceNumber(1));
		assertFalse(new SequenceNumber(1).equals(new SequenceNumber(2)));
		assertFalse(new SequenceNumber(1).equals(new SequenceNumber(0)));
		assertEquals(new SequenceNumber(1,2,3), new SequenceNumber(1,2,3));
		assertFalse(new SequenceNumber(1,2,3).equals(new SequenceNumber(1,2,30)));
		assertFalse(new SequenceNumber(1,2,3).equals(new SequenceNumber(1,2)));
		assertFalse(new SequenceNumber(1,2).equals(new SequenceNumber(1,2,3)));
	}
	public void testHashCode() {
		SequenceNumber s = new SequenceNumber(1);
		assertEquals(s.hashCode(), s.hashCode());
		assertEquals(s.hashCode(), new SequenceNumber(1).hashCode());
		assertEquals(s.hashCode(), new SequenceNumber(1,0,0).hashCode());
		assertFalse(s.hashCode() == new SequenceNumber(2).hashCode());
		assertFalse(s.hashCode() == new SequenceNumber(0).hashCode());
		assertFalse(s.hashCode() == new SequenceNumber(1,2,3).hashCode());
		assertEquals(new SequenceNumber(1,2,3).hashCode(), new SequenceNumber(1,2,3).hashCode());
		assertFalse(new SequenceNumber(1,2,3).hashCode() == new SequenceNumber(1,2,30).hashCode());		
	}
	public void testIncrementLast() {
		assertEquals(new SequenceNumber(1).incrementLast().toIntArray(), new int[]{2});
		assertEquals(new SequenceNumber(1, 2).incrementLast().toIntArray(), new int[]{1,3});
	}
	public void testCompareTo() {
		assertEquals(new SequenceNumber(1).compareTo(new SequenceNumber(2)), -1);
		assertEquals(new SequenceNumber(1).compareTo(new SequenceNumber(1)), 0);
		assertEquals(new SequenceNumber(1).compareTo(new SequenceNumber(0)), 1);
		assertEquals(new SequenceNumber(1).compareTo(new SequenceNumber(1,1)), -1);
		assertEquals(new SequenceNumber(1).compareTo(new SequenceNumber(1,0)), 0);
		assertEquals(new SequenceNumber(1,2,3).compareTo(new SequenceNumber(1,2,3)), 0);		
		assertEquals(new SequenceNumber(new int[]{2,2,1,1,1}).compareTo(new SequenceNumber(new int[]{2,2,2})), -1);		
	}
	public void testIsBranchingBefore() {
		assertTrue(new SequenceNumber(1).isBranchingBefore(new SequenceNumber(2)));
		assertFalse(new SequenceNumber(1).isBranchingBefore(new SequenceNumber(1)));
		assertFalse(new SequenceNumber(1).isBranchingBefore(new SequenceNumber(0)));
		assertTrue(new SequenceNumber(1).isBranchingBefore(new SequenceNumber(1,1)));
		assertTrue(new SequenceNumber(1).isBranchingBefore(new SequenceNumber(1,0)));
		assertTrue(new SequenceNumber(new int[]{2,2,1}).isBranchingBefore(new SequenceNumber(new int[]{2,2,2})));
		assertTrue(new SequenceNumber(new int[]{2,2,1}).isBranchingBefore(new SequenceNumber(new int[]{2,2,1,1,1})));
		assertFalse(new SequenceNumber(new int[]{2,2,1,1,1}).isBranchingBefore(new SequenceNumber(new int[]{2,2,2})));
		assertFalse(new SequenceNumber(new int[]{2,2,1,1,1}).isBranchingBefore(new SequenceNumber(new int[]{2,2,2,1,1})));
		assertFalse(new SequenceNumber(1,2,3).isBranchingBefore(new SequenceNumber(1,3,1)));
		assertFalse(new SequenceNumber(1,2,3).isBranchingBefore(new SequenceNumber(1,2,3)));
	}
	public void testIsBranchingBeforeOrEqual() {
		assertTrue(new SequenceNumber(1).isBranchingBeforeOrEqual(new SequenceNumber(2)));
		assertTrue(new SequenceNumber(1).isBranchingBeforeOrEqual(new SequenceNumber(1)));
		assertFalse(new SequenceNumber(1).isBranchingBeforeOrEqual(new SequenceNumber(0)));
		assertTrue(new SequenceNumber(1).isBranchingBeforeOrEqual(new SequenceNumber(1,1)));
		assertTrue(new SequenceNumber(1).isBranchingBeforeOrEqual(new SequenceNumber(1,0)));
		assertTrue(new SequenceNumber(new int[]{2,2,1}).isBranchingBeforeOrEqual(new SequenceNumber(new int[]{2,2,2})));
		assertTrue(new SequenceNumber(new int[]{2,2,1}).isBranchingBeforeOrEqual(new SequenceNumber(new int[]{2,2,1,1,1})));
		assertFalse(new SequenceNumber(new int[]{2,2,1,1,1}).isBranchingBeforeOrEqual(new SequenceNumber(new int[]{2,2,2})));
		assertFalse(new SequenceNumber(new int[]{2,2,1,1,1}).isBranchingBeforeOrEqual(new SequenceNumber(new int[]{2,2,2,1,1})));
		assertFalse(new SequenceNumber(1,2,3).isBranchingBeforeOrEqual(new SequenceNumber(1,3,1)));
		assertTrue(new SequenceNumber(1,2,3).isBranchingBeforeOrEqual(new SequenceNumber(1,2,3)));
	}
}
