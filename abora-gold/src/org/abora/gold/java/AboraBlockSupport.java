package org.abora.gold.java;



public class AboraBlockSupport {

	public AboraBlockSupport() {
		super();
	}
	
	/**
	 * @see org.abora.gold.snarf.DiskManager consitent
	 */
	public static void enterConsistent() {
		
		enterConsistent(-1);
	}
	
	public static void enterConsistent(int dirty) {
		throw new UnsupportedOperationException();
	}
	
	public static void exitConsistent() {
		throw new UnsupportedOperationException();
	}

}
