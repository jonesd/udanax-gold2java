package org.abora.gold.java.exception;




public class UnimplementedException extends AboraRuntimeException {

	private static final long serialVersionUID = 1L;

	public UnimplementedException() {
		super();
	}

	public UnimplementedException(String message) {
		super(message);
	}

	public UnimplementedException(String message, Throwable cause) {
		super(message, cause);
	}

	public UnimplementedException(Throwable cause) {
		super(cause);
	}

}
