/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.ent;

/**
 */
public class NonSameBranchException extends EntException {

	/**
	 * Constructor for NonSameBranchException.
	 */
	public NonSameBranchException() {
		super();
	}

	/**
	 * Constructor for NonSameBranchException.
	 * @param message
	 */
	public NonSameBranchException(String message) {
		super(message);
	}

	/**
	 * Constructor for NonSameBranchException.
	 * @param message
	 * @param cause
	 */
	public NonSameBranchException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for NonSameBranchException.
	 * @param cause
	 */
	public NonSameBranchException(Throwable cause) {
		super(cause);
	}

}
