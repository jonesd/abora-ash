/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.engine;

/**
 */
public class AboraConverter {

	/**
	 * Constructor for AboraConverter.
	 */
	private AboraConverter() {
		super();
	}
	
	public static byte[] toAboraContent(String s) {
		return s.getBytes();
	}

	public static String toJavaString(byte[] bytes) {
		return new String(bytes);
	}

}
