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


/**
 */
public class IntegerRegion extends Region {
	private final int startPosition;
	private final int extent;
	private static final IntegerRegion EMPTY_REGION = new IntegerRegion(0, 0);

	private IntegerRegion(int startPosition, int extent) {
		this.startPosition = startPosition;
		this.extent = extent;
	}
	public static IntegerRegion startExtent(int startPosition, int extent) {
		return new IntegerRegion(startPosition, extent);
	}
	public static IntegerRegion startEnd(int startPosition, int endPosition) {
		return new IntegerRegion(startPosition, endPosition - startPosition + 1);
	}
	public static IntegerRegion empty() {
		return EMPTY_REGION;
	}

	public boolean equals(Object o) {
		if (!(o instanceof IntegerRegion))
			return false;
		IntegerRegion r = (IntegerRegion) o;
		return r.getStartPosition() == getStartPosition() && r.getExtent() == getExtent();
	}
	public int getExtent() {
		return extent;
	}

	public int getStartPosition() {
		return startPosition;
	}
	public int getBeyondPosition() {
		return getStartPosition() + getExtent();
	}
	public String asString() {
		return "IntegerRegion[" + getStartPosition() + ", " + getEndPosition() + "]";
	}
	//	do: operation
	//		^self startPosition to: self endPosition do: [:position | operation value: position]!
	public int getEndPosition() {
		return getStartPosition() + getExtent() - 1;
	}
	public int hashCode() {
		return getStartPosition() << 1 | getExtent();
	}
	public boolean includes(int position) {
		final int relativePosition = relativePosition(position);
		return relativePosition >= 0 && relativePosition < getExtent();
	}
	/**
	 * Answer a region with positions the receiver and anotherRegion have in common.
	 */
	public IntegerRegion intersection(IntegerRegion anotherRegion) {
		if (intersects(anotherRegion)) {
			int newStart = Math.max(getStartPosition(), anotherRegion.getStartPosition());
			int newEnd = Math.min(getEndPosition(), anotherRegion.getEndPosition());
			return IntegerRegion.startEnd(newStart, newEnd);
		} else {
			return IntegerRegion.empty();
		}
	}
	/*
	 * Answer whether any position of the receiver can be found in anotherRegion.
	 */
	public boolean intersects(IntegerRegion anotherRegion) {
		return getStartPosition() <= anotherRegion.getEndPosition()
			&& getEndPosition() >= anotherRegion.getStartPosition();
	}
	public boolean isEmpty() {
		return getExtent() <= 0;
	}
	public int relativePosition(int position) {
		return position - getStartPosition();
	}
}
