/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.fluid;

import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.BlockClosure;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;
import info.dgjones.abora.gold.xpp.fluid.FluidPromiseVar;
import info.dgjones.abora.gold.xpp.fluid.FluidVar;

/**
 * We have to use the name of the actual fluid variable since the variable itself may not be
 * initialized when its corresponding promise fluid variable is initialized.  Since this
 * happens during
 * static init time, the order cannot be controlled.
 */
public class FluidPromiseVar extends FluidVar {

	protected String myActualFluid;
/*
Xanadu-Xpp-fluid.st:304:
FluidVar subclass: #FluidPromiseVar
	instanceVariableNames: 'myActualFluid {Symbol}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!
*/
/*
Xanadu-Xpp-fluid.st:310:
FluidPromiseVar comment:
'We have to use the name of the actual fluid variable since the variable itself may not be
initialized when its corresponding promise fluid variable is initialized.  Since this happens during
static init time, the order cannot be controlled.'!
*/
/*
Xanadu-Xpp-fluid.st:339:
FluidPromiseVar class
	instanceVariableNames: ''!
*/
public FluidPromiseVar(BlockClosure initialBlock, Emulsion emulsion, String name, AboraClass classx, FluidVar actual) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:318:FluidPromiseVar methodsFor: 'create'!
create: initialBlock {BlockClosure} 
	with: emulsion {Emulsion} 
	with: name {Symbol} 
	with: class {Class}
	with: actual {FluidVar}
	
	super create: initialBlock with: emulsion with: name with: class.
	actual == nil ifTrue: [self halt].
	myActualFluid := actual!
*/
}
public void fluidSet(Object newValue) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:330:FluidPromiseVar methodsFor: 'accessing'!
fluidSet: newValue
	
	super fluidSet: newValue.
	(newValue == NULL or: [newValue isBroken]) ifTrue:
		[(Smalltalk at: myActualFluid) fluidSet: NULL]
	ifFalse:
		[(Smalltalk at: myActualFluid) fluidSet: newValue actualThing]!
*/
}
/*
Xanadu-Xpp-fluid.st:357:FluidPromiseVar class methodsFor: 'make'!
Emulsion initialize!
*/
public static FluidPromiseVar make(String varName, BlockClosure initialValueBlock, Emulsion emulsion, AboraClass classx, FluidVar actual) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:346:FluidPromiseVar class methodsFor: 'make'!
make: varName {String} 
	with: initialValueBlock {BlockClosure | nil} 
	with: emulsion {Emulsion}
	with: class {Class}
	with: actual {FluidVar}
	
	(Smalltalk at: varName asSymbol ifAbsent: []) isBehavior 
		ifTrue: [self error: varName, 'cannot be used as a fluid because it conflicts with a class name.'].
	Smalltalk safeAt: varName asSymbol put:
		(self create: initialValueBlock with: emulsion with: varName asSymbol with: class with: actual)!
*/
}
public FluidPromiseVar() {
/*

Generated during transformation
*/
}
public FluidPromiseVar(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
