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
package info.dgjones.abora.ash.engine;

/**
 */
public class Id extends AboraObject {
	private final long value;
	private static long NEXT_VALUE= 1;
	
	public Id() {
		value = NEXT_VALUE++;
	}
	public Id(AboraObject o) {
		this();
		WorksStorage.getInstance().put(this, o);
	}

	public boolean equals(Object o) {
		if (!(o instanceof Id))
			return false;
		Id id = (Id) o;
		return getValue() == id.getValue();
	}
	public int hashCode() {
		return (int)getValue();
	}
	public long getValue() {
		return value;
	}
	public String asString() {
		return "Id("+getValue()+")";
	}
//	nextId
//		| id |
//		id := (self new)
//					value: NextValue;
//					yourself.
//		NextValue := NextValue + 1.
//		^id!
//
//	nextIdFor: anObject
//		| id |
//		id := self nextId.
//		WorksStorage current addId: id with: anObject.
//		^id! !
}
