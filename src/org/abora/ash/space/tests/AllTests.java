/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.space.tests;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 */
public class AllTests extends TestCase {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.be.space.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(IntegerRegionTest.class));
		//$JUnit-END$
		return suite;
	}
}
