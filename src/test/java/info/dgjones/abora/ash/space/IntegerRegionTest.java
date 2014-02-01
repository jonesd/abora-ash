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
package info.dgjones.abora.ash.space;

import info.dgjones.abora.ash.space.IntegerRegion;

import junit.framework.TestCase;

/**
 */
public class IntegerRegionTest extends TestCase {

	public IntegerRegionTest(String arg0) {
		super(arg0);
	}

public void testBeyondPosition() {
	IntegerRegion region = IntegerRegion.startExtent(10, 3);
	assertEquals(region.getBeyondPosition(), 13);
}
public void testNew() {
	IntegerRegion region = IntegerRegion.startExtent(10, 2);
	assertEquals(region.getStartPosition(), 10);
	assertEquals(region.getExtent(), 2);
}
public void testNew2() {
	IntegerRegion region = IntegerRegion.startEnd(10, 11);
	assertEquals(region.getStartPosition(), 10);
	assertEquals(region.getExtent(), 2);
}
public void testAsString() {
	assertEquals(IntegerRegion.startEnd(10, 11).asString(), "IntegerRegion[10, 11]");
	assertEquals(IntegerRegion.startExtent(10, 1).asString(), "IntegerRegion[10, 10]");
}
public void testEndPosition() {
	IntegerRegion region = IntegerRegion.startExtent(10, 3);
	assertEquals(region.getEndPosition(), 12);
}
public void testEquals() {
	IntegerRegion region = IntegerRegion.startExtent(10, 2);
	assertEquals(region, region);
	assertEquals(region, IntegerRegion.startExtent(10, 2));
	
	assertFalse(region.equals(IntegerRegion.startExtent(10, 3)));
	assertFalse(region.equals(IntegerRegion.startExtent(10, 1)));
	assertFalse(region.equals(IntegerRegion.startExtent(9, 2)));
	assertFalse(region.equals(IntegerRegion.startExtent(9, 3)));
	assertFalse(region.equals(IntegerRegion.startExtent(10, 0)));
	
}
public void testHashCode() {
	IntegerRegion region = IntegerRegion.startExtent(10, 2);
	assertEquals(region.hashCode(), region.hashCode());
	assertEquals(region.hashCode(), IntegerRegion.startExtent(10, 2).hashCode());
	assertFalse(region.hashCode() == IntegerRegion.startExtent(10, 3).hashCode());
	assertFalse(region.hashCode() == IntegerRegion.startExtent(10, 0).hashCode());
}
public void testIncludes() {
	IntegerRegion region = IntegerRegion.startExtent(10, 3);
	assertTrue(region.includes(10));
	assertTrue(region.includes(11));
	assertTrue(region.includes(12));
	assertFalse(region.includes(9));
	assertFalse(region.includes(13));
	
}
public void testIntersection() {
	IntegerRegion region = IntegerRegion.startExtent(10, 3);
	assertEquals(region.intersection(region), region);
	assertEquals(region.intersection(IntegerRegion.startExtent(9, 5)), IntegerRegion.startExtent(10, 3));
	assertEquals(region.intersection(IntegerRegion.startExtent(9, 2)), IntegerRegion.startExtent(10, 1));
	assertEquals(region.intersection(IntegerRegion.startExtent(12, 4)), IntegerRegion.startExtent(12, 1));
	assertEquals(region.intersection(IntegerRegion.startExtent(11, 1)), IntegerRegion.startExtent(11, 1));

	assertEquals(region.intersection(IntegerRegion.startExtent(9, 1)), IntegerRegion.empty());
	assertEquals(region.intersection(IntegerRegion.startExtent(13, 4)), IntegerRegion.empty());
}
public void testIntersects() {
	IntegerRegion region = IntegerRegion.startExtent(10, 3);
	assertTrue(region.intersects(region));
	assertTrue(region.intersects(IntegerRegion.startExtent(9, 5)));
	assertTrue(region.intersects(IntegerRegion.startExtent(9, 2)));
	assertTrue(region.intersects(IntegerRegion.startExtent(12, 4)));
	assertTrue(region.intersects(IntegerRegion.startExtent(11, 1)));

	assertFalse(region.intersects(IntegerRegion.startExtent(9, 1)));
	assertFalse(region.intersects(IntegerRegion.startExtent(13, 4)));
	
	assertFalse(region.intersects(IntegerRegion.empty()));
}
public void testIsEmpty() {
	assertTrue(IntegerRegion.startExtent(10, 0).isEmpty());
	assertTrue(IntegerRegion.startExtent(10, -1).isEmpty());
	assertFalse(IntegerRegion.startExtent(10, 1).isEmpty());
}
public void testRelativePosition() {
	IntegerRegion region = IntegerRegion.startExtent(10, 3);
	assertEquals(region.relativePosition(9), -1);
	assertEquals(region.relativePosition(10), 0);
	assertEquals(region.relativePosition(11), 1);
	assertEquals(region.relativePosition(12), 2);
	assertEquals(region.relativePosition(13), 3);
	assertEquals(region.relativePosition(14), 4);
}

}
