package org.abora.gold.java;



public class PasseException extends AboraRuntimeException {

	private static final long serialVersionUID = 1L;

	public PasseException() {
		super();
	}

	public PasseException(String message) {
		super(message);
	}

	public PasseException(String message, Throwable cause) {
		super(message, cause);
	}

	public PasseException(Throwable cause) {
		super(cause);
	}

}
