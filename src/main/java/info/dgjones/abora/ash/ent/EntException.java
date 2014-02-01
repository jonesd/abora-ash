/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package info.dgjones.abora.ash.ent;


/**
 */
public class EntException extends RuntimeException {

	/**
	 * Constructor for EntException.
	 */
	public EntException() {
		super();
	}

	/**
	 * Constructor for EntException.
	 * @param message
	 */
	public EntException(String message) {
		super(message);
	}

	/**
	 * Constructor for EntException.
	 * @param message
	 * @param cause
	 */
	public EntException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for EntException.
	 * @param cause
	 */
	public EntException(Throwable cause) {
		super(cause);
	}

}
