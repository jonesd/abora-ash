/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.ent;

/**
 */
public class NonCompatibleBranchException extends EntException {

	/**
	 * Constructor for NonCompatibleBranchException.
	 */
	public NonCompatibleBranchException() {
		super();
	}

	/**
	 * Constructor for NonCompatibleBranchException.
	 * @param message
	 */
	public NonCompatibleBranchException(String message) {
		super(message);
	}

	/**
	 * Constructor for NonCompatibleBranchException.
	 * @param message
	 * @param cause
	 */
	public NonCompatibleBranchException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for NonCompatibleBranchException.
	 * @param cause
	 */
	public NonCompatibleBranchException(Throwable cause) {
		super(cause);
	}

}
