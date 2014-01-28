/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.java;

import java.util.ArrayList;
import java.util.List;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xpp.fluid.FluidVar;



public class AboraBlockSupport {

	public AboraBlockSupport() {
		super();
	}
	
	/**
	 * @see info.dgjones.abora.gold.snarf.DiskManager consitent
	 */
	public static void enterConsistent() {
		
		enterConsistent(-1);
	}
	
	
	//TODO should return a context object to caller for exit
	private static final List consistentDirties = new ArrayList();
	public static void enterConsistent(int dirty) {
		DiskManager diskManager = (DiskManager) AboraHeaper.CurrentPacker.fluidGet();
		diskManager.beginConsistent(dirty);
		consistentDirties.add(new Integer(dirty));
		//TODO Can these be nested?
		AboraHeaper.InsideTransactionFlag.fluidSet(Boolean.TRUE);
	}
	
	public static void exitConsistent() {
		int dirty = ((Integer)consistentDirties.remove(consistentDirties.size()-1)).intValue();
		DiskManager diskManager = (DiskManager) AboraHeaper.CurrentPacker.fluidGet();
		if (consistentDirties.isEmpty()) {
			AboraHeaper.InsideTransactionFlag.fluidSet(Boolean.FALSE);
		}
		diskManager.endConsistent(dirty);
	}

	public static void enterConsistent(int dirty, DiskManager diskManager) {
		throw new UnsupportedOperationException();
	}
	
	public static void exitConsistent(DiskManager diskManager) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see info.dgjones.abora.gold.snarf.DiskManager insistent
	 */
	public static void enterInsistent() {
		
		enterInsistent(-1);
	}
	
	public static void enterInsistent(int dirty) {
		if (!((Boolean)AboraHeaper.InsideTransactionFlag.fluidGet()).booleanValue()) {
			throw new AboraRuntimeException("Must be inside a transaction");
		}
		enterConsistent(dirty);
	}
	
	public static void exitInsistent() {
		exitConsistent();
	}
	/**
	 * @see FluidVar fluidBindDuring
	 */
	static class FluidBindContext {
		public Object old;
		public Object oldSpace;
	}
	public static Object enterFluidBindDuring(FluidVar fluidVar, Object newValue) {
//		old _ self fluidFetch.
//		oldSpace _ myEmulsion fetchOldRawSpace.
//		self fluidSet: value.
//		result _ dynamicBlock
//			valueNowOrOnUnwindDo: 
//				[self fluidSet: old].
//		myEmulsion fetchOldRawSpace == oldSpace assert: 'Emulsion space switched during fluidBind'.
//		^ result!

		FluidBindContext context = new FluidBindContext();
		context.old = fluidVar.fluidFetch();
		context.oldSpace = fluidVar.emulsion().fetchOldRawSpace();
		
		fluidVar.fluidSet(newValue);
		
		return context;
	}
	public static Object enterFluidBindDuring(FluidVar fluidVar, boolean newValue) {
		return enterFluidBindDuring(fluidVar, newValue ? Boolean.TRUE : Boolean.FALSE);
	}
	
	public static void exitFluidBindDuring(FluidVar fluidVar, Object oldVar) {
		FluidBindContext context = (FluidBindContext)oldVar;
		fluidVar.fluidSet(context.old);
		if (fluidVar.emulsion().fetchOldRawSpace() != context.oldSpace) {
			throw new AboraAssertionException("Emulsion space switched during fluidBind");
		}
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
