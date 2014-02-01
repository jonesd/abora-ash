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

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.WeakHashMap;

import info.dgjones.abora.ash.content.BeWork;

/**
 */
public class WorksStorage extends AboraObject {
	private final Map idLookup = new WeakHashMap();
	private final Set works = new HashSet();
	private BeWork userHome = null;
	private static final WorksStorage INSTANCE = new WorksStorage(); 

//		instanceVariableNames: 'works idLookup userHome'

	public void put(Id id, AboraObject value) {
		//@todo this should probably be somewhere else
		idLookup.put(id, new WeakReference(value));
	}
	public void add(BeWork work) {
		works.add(work);
	}
	public int knownIds() {
		return idLookup.size();
	}
	public AboraObject get(Id id) {
		//@todo this should probably be somewhere else
		WeakReference holder = (WeakReference) idLookup.get(id);
		AboraObject value = holder != null ? (AboraObject) holder.get() : null;
		if (value == null)
			throw new NoSuchElementException("Nothing associated with: "+id);
		return value;
	}
	public void remove(BeWork work) {
		works.remove(work);
	}
//
//	works
//		^works
	public static WorksStorage getInstance() {
		return INSTANCE;
	}

	/**
	 * Returns the userHome.
	 * @return BeWork
	 */
	public BeWork getUserHome() {
		return userHome;
	}

	/**
	 * Sets the userHome.
	 * @param userHome The userHome to set
	 */
	public void setUserHome(BeWork userHome) {
		this.userHome = userHome;
	}

}