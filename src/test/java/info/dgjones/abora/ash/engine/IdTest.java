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

import java.util.HashSet;
import java.util.Set;

import info.dgjones.abora.ash.engine.Id;

import junit.framework.TestCase;

/**
 */
public class IdTest extends TestCase {

	/**
	 * Constructor for IdTest.
	 * @param arg0
	 */
	public IdTest(String arg0) {
		super(arg0);
	}
	
	public void testNew() {
		Id id = new Id();
		assertNotNull(id);
	}
	public void testNewUniqueness() {
		Set values = new HashSet();
		for (int i = 0; i < 20; i++) {
			Id id = new Id();
			Long value = new Long(id.getValue());
			assertFalse(values.contains(value));
			values.add(value);
		}
	}
	public void testEquals() {
		Id id = new Id();
		assertEquals(id, id);
		assertFalse(id.equals(new Id()));
	}
}
