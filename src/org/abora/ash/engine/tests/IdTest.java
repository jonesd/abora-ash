/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.engine.tests;

import java.util.HashSet;
import java.util.Set;

import org.abora.ash.engine.Id;

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
