package org.abora.gold.java.exception;



public class ShouldImplementException extends AboraRuntimeException {

	private static final long serialVersionUID = 1L;

	public ShouldImplementException() {
		super();
	}

	public ShouldImplementException(String message) {
		super(message);
	}

	public ShouldImplementException(String message, Throwable cause) {
		super(message, cause);
	}

	public ShouldImplementException(Throwable cause) {
		super(cause);
	}

}
