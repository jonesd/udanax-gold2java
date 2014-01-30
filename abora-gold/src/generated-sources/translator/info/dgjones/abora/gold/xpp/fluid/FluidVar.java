/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.fluid;

import info.dgjones.abora.gold.java.AboraHeaper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.BlockClosure;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;
import info.dgjones.abora.gold.xpp.fluid.FluidVar;
import java.io.PrintWriter;

public class FluidVar extends AboraHeaper {

	protected BlockClosure myInitBlock;
	protected Emulsion myEmulsion;
	protected int myOffset;
	protected String myName;
	protected AboraClass myType;
/*
Xanadu-Xpp-fluid.st:162:
Object subclass: #FluidVar
	instanceVariableNames: '
		myInitBlock {BlockClosure}
		myEmulsion {Emulsion}
		myOffset {Integer}
		myName {Symbol}
		myType {Class}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!
*/
/*
Xanadu-Xpp-fluid.st:258:
FluidVar class
	instanceVariableNames: ''!
*/
public Emulsion emulsion() {
	return myEmulsion;
/*
Xanadu-Xpp-fluid.st:177:FluidVar methodsFor: 'private: accessing'!
emulsion
	^ myEmulsion!
*/
}
/**
 * Return the fluidly bound value.
 */
public Object fluidVar() {
	return myEmulsion.fluidsSpace().at(myOffset);
/*
Xanadu-Xpp-fluid.st:182:FluidVar methodsFor: 'obsolete'!
fluidVar
	"Return the fluidly bound value."
	^ myEmulsion fluidsSpace at: myOffset!
*/
}
/*
Xanadu-Xpp-fluid.st:188:FluidVar methodsFor: 'accessing'!
fluidBind: value during: dynamicBlock
	"Take on new definition during execution of a block, and assure that old value is restored.
	For the benefit of smalltalk only code that may want to use this in an expression, the result of the block evaluation is returned."
	| old oldSpace result |
	old _ self fluidFetch.
	oldSpace _ myEmulsion fetchOldRawSpace.
	self fluidSet: value.
	result _ dynamicBlock
		valueNowOrOnUnwindDo: 
			[self fluidSet: old].
	myEmulsion fetchOldRawSpace == oldSpace assert: 'Emulsion space switched during fluidBind'.
	^ result!
*/
/**
 * Return the fluidly bound value.
 */
public Object fluidFetch() {
	return myEmulsion.fluidsSpace().at(myOffset);
/*
Xanadu-Xpp-fluid.st:201:FluidVar methodsFor: 'accessing'!
fluidFetch
	"Return the fluidly bound value."
	^ myEmulsion fluidsSpace at: myOffset!
*/
}
/**
 * Return the fluidly bound value.
 */
public Object fluidGet() {
	if (fluidFetch() == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULLFLUID);
	}
	return fluidFetch();
/*
Xanadu-Xpp-fluid.st:205:FluidVar methodsFor: 'accessing'!
fluidGet
	"Return the fluidly bound value."
	self fluidFetch == NULL ifTrue: [Heaper BLAST: #NULLFluid].
	^self fluidFetch!
*/
}
/**
 * self == CurrentPacker ifTrue: [self halt].
 */
public void fluidSet(Object newValue) {
	/* assign a new value to the fluid variable */
	if (myEmulsion.fluidsSpace() != null) {
		myEmulsion.fluidsSpace().put(myOffset, newValue);
	}
/*
Xanadu-Xpp-fluid.st:210:FluidVar methodsFor: 'accessing'!
fluidSet: newValue
	"self == CurrentPacker ifTrue: [self halt]."
	"assign a new value to the fluid variable"
	myEmulsion fluidsSpace ~~ nil ifTrue: [myEmulsion fluidsSpace at: myOffset put: newValue]!
*/
}
/**
 * called by Emulsion fluidsSpace when a new space is acquired
 */
public void init() {
	fluidSet(initialValue());
/*
Xanadu-Xpp-fluid.st:215:FluidVar methodsFor: 'accessing'!
{void} init
	"called by Emulsion fluidsSpace when a new space is acquired"
	self fluidSet: self initialValue!
*/
}
public Object initialValue() {
	return (myInitBlock != null) ? myInitBlock.value() : null;
/*
Xanadu-Xpp-fluid.st:219:FluidVar methodsFor: 'accessing'!
{Object} initialValue
	^ myInitBlock ~~ nil ifTrue: [myInitBlock value] ifFalse: [nil].!
*/
}
/**
 * This create the first fluid variable within the emulsion
 */
public FluidVar(BlockClosure initialBlock, Emulsion emulsion, String name, AboraClass classx) {
	super();
	myInitBlock = initialBlock;
	if ( ! (emulsion != null)) {
		throw new AboraAssertionException("Must have an emulsion");
	}
	myEmulsion = emulsion;
	myOffset = myEmulsion.addFluid(this);
	myName = name;
	myType = classx;
/*
Xanadu-Xpp-fluid.st:224:FluidVar methodsFor: 'create'!
create: initialBlock {BlockClosure} with: emulsion {Emulsion} with: name {Symbol} with: class {Class}
	"This create the first fluid variable within the emulsion"
	super create.
	myInitBlock _ initialBlock.
	emulsion ~~ NULL assert: 'Must have an emulsion'.
	myEmulsion _ emulsion.
	myOffset _ myEmulsion addFluid: self.
	myName _ name.
	myType _ class.!
*/
}
public void printOn(PrintWriter aStream) {
	aStream.print(getAboraClass().name());
	aStream.print("(");
	if (myEmulsion != null) {
		aStream.print(AboraSupport.findCategory(myEmulsion.getClass()));
		aStream.print("/");
		aStream.print(myName);
		aStream.print("{");
		aStream.print(myType);
		aStream.print("}");
		aStream.print(",  ");
		aStream.print(myOffset);
		if (myOffset != 0) {
			/* aStream << '->' << (myEmulsion fluidsSpace at: myOffset) */
			;
		}
		else {
			aStream.print("->nil myOffset");
		}
	}
	else {
		aStream.print("???????");
	}
	aStream.print(")");
/*
Xanadu-Xpp-fluid.st:236:FluidVar methodsFor: 'printing'!
printOn: aStream
	aStream << self getCategory name << '('.
	myEmulsion ~~ nil
	ifTrue:
		[aStream << myEmulsion class << '/' << myName
		<< '{' << myType << '}' << ',  ' << myOffset.
		myOffset ~~ nil
		ifTrue: ["aStream << '->' << (myEmulsion fluidsSpace at: myOffset)"]
		ifFalse: [aStream << '->nil myOffset']]
	ifFalse:
		[aStream << '???????'].
	aStream << ')'!
*/
}
public void goAway() {
	myEmulsion = null;
	myInitBlock = null;
	if (myName != null) {
		Smalltalk.safeAtPut(myName, null);
	}
/*
Xanadu-Xpp-fluid.st:251:FluidVar methodsFor: 'special'!
goAway
	myEmulsion _ nil.
	myInitBlock _ nil.
	myName ~~ nil ifTrue:
		[Smalltalk safeAt: myName asSymbol put: nil].!
*/
}
/**
 * If there is a fluidVar by the given name, set its value to nil
 */
public static void cleanup(String varName) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:265:FluidVar class methodsFor: 'cleanup'!
cleanup: varName {Symbol}
	"If there is a fluidVar by the given name, set its value to nil"
	| var |
	var _ Smalltalk at: varName asSymbol ifAbsent: [].
	(var isKindOf: FluidVar)
		ifTrue:
			[(var emulsion isKindOf: Emulsion)
				ifTrue: [var fluidSet: nil]
				ifFalse: [Transcript show: varName asString, ' has no emulsion.']]!
*/
}
/**
 * Reset all fluid variables to their initial states.
 */
public static void cleanupGarbage() {
	/* FluidVar cleanupGarbage. */
	OrderedCollection allInstances = AboraSupport.allInstances(FluidVar.class);
	for (int doIndex = 0; doIndex < allInstances.size(); doIndex ++ ) {
		FluidVar fluid = (FluidVar) allInstances.get(doIndex);
		if (fluid.emulsion() != Emulsion.imageEmulsion()) {
			fluid.goAway();
		}
	}
/*
Xanadu-Xpp-fluid.st:275:FluidVar class methodsFor: 'cleanup'!
{void} cleanupGarbage
	"Reset all fluid variables to their initial states."
	"FluidVar cleanupGarbage."
	
	FluidVar allInstancesDo: [:fluid |
		fluid emulsion ~~ Emulsion imageEmulsion ifTrue: [fluid goAway]]!
*/
}
public static FluidVar make(String varName, BlockClosure initialValueBlock, Emulsion emulsion, AboraClass classx) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
Xanadu-Xpp-fluid.st:284:FluidVar class methodsFor: 'make'!
make: varName {String} 
	with: initialValueBlock {BlockClosure | nil} 
	with: emulsion {Emulsion}
	with: class {Class}
	
	(Smalltalk at: varName asSymbol ifAbsent: []) isBehavior 
		ifTrue: [self error: varName, 'cannot be used as a fluid because it conflicts with a class name.'].
	Smalltalk safeAt: varName asSymbol put:
		(self create: initialValueBlock with: emulsion with: varName asSymbol with: class)!
*/
}
/**
 * @deprecated
 */
public static FluidVar make(String varName) {
	throw new PasseException();
/*
Xanadu-Xpp-fluid.st:296:FluidVar class methodsFor: 'smalltalk: passe'!
make: varName {String}
	
	self passe!
*/
}
/**
 * @deprecated
 */
public static FluidVar make(String varName, Heaper initialValue) {
	throw new PasseException();
/*
Xanadu-Xpp-fluid.st:300:FluidVar class methodsFor: 'smalltalk: passe'!
make: varName {String} with: initialValue {Heaper | NULL}
	
	self passe!
*/
}
public FluidVar() {
/*

Generated during transformation
*/
}
public FluidVar(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
