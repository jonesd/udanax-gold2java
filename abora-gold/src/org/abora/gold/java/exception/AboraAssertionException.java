package org.abora.gold.java.exception;



public class AboraAssertionException extends AboraRuntimeException {

	private static final long serialVersionUID = 1L;

	public AboraAssertionException() {
		super();
	}

	public AboraAssertionException(String message) {
		super(message);
	}

	public AboraAssertionException(String message, Throwable cause) {
		super(message, cause);
	}

	public AboraAssertionException(Throwable cause) {
		super(cause);
	}

}
