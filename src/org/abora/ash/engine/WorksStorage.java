/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.engine;

import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.WeakHashMap;

import org.abora.ash.content.BeWork;

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