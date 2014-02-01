/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package info.dgjones.abora.ash.engine;

/**
 */
public class AboraException extends Exception {

	/**
	 * Constructor for AboraException.
	 */
	public AboraException() {
		super();
	}

	/**
	 * Constructor for AboraException.
	 * @param message
	 */
	public AboraException(String message) {
		super(message);
	}

	/**
	 * Constructor for AboraException.
	 * @param message
	 * @param cause
	 */
	public AboraException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for AboraException.
	 * @param cause
	 */
	public AboraException(Throwable cause) {
		super(cause);
	}

}
