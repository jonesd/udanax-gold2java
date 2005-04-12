package org.abora.gold.java;

import org.abora.gold.backrec.ResultRecorder;
import org.abora.gold.fossil.RecorderFossil;
import org.abora.gold.java.missing.smalltalk.BlockClosure;
import org.abora.gold.snarf.DiskManager;
import org.abora.gold.xpp.fluid.FluidVar;



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

	public static void enterConsistent(int dirty, DiskManager diskManager) {
		throw new UnsupportedOperationException();
	}
	
	public static void exitConsistent(DiskManager diskManager) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see org.abora.gold.snarf.DiskManager insistent
	 */
	public static void enterInsistent() {
		
		enterInsistent(-1);
	}
	
	public static void enterInsistent(int dirty) {
		throw new UnsupportedOperationException();
	}
	
	public static void exitInsistent() {
		throw new UnsupportedOperationException();
	}
	/**
	 * @see FluidVar fluidBindDuring
	 */
	public static Object enterFluidBindDuring(FluidVar fluidVar, Object newValue) {
		throw new UnsupportedOperationException();
	}
	
	public static void exitFluidBindDuring(FluidVar fluidVar, Object oldVar) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * @see RecorderFossil reanimate
	 */
	public static ResultRecorder enterRecorderFossilReanimate(RecorderFossil fossil) {
		throw new UnsupportedOperationException();
	}
	
	public static void exitRecorderFossilReanimate() {
		throw new UnsupportedOperationException();
	}

}
