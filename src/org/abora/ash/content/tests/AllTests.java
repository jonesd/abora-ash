/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */
package org.abora.ash.content.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 */
public class AllTests extends BeCollectionHolderTest {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.ash.content.tests");
		//$JUnit-BEGIN$
		suite.addTestSuite(BeCollectionHolderTest.class);
		//$JUnit-END$
		return suite;
	}
}
