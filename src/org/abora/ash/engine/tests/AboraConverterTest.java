/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */
package org.abora.ash.engine.tests;

import org.abora.ash.engine.AboraConverter;

import junit.framework.TestCase;

/**
 */
public class AboraConverterTest extends TestCase {
	
	public void testToAboraContent() {
		String s = "hello";
		byte[] converted = AboraConverter.toAboraContent(s);
		assertEquals(s, AboraConverter.toJavaString(converted));
	}

}
