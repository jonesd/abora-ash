/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.tests;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 */
public class AllTests extends TestCase {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.content.tests");
		//$JUnit-BEGIN$
		suite.addTest(org.abora.ash.content.tests.AllTests.suite());
		suite.addTest(org.abora.ash.engine.tests.AllTests.suite());
		suite.addTest(org.abora.ash.ent.tests.AllTests.suite());
		suite.addTest(org.abora.ash.space.tests.AllTests.suite());
		//$JUnit-END$
		return suite;
	}
}
