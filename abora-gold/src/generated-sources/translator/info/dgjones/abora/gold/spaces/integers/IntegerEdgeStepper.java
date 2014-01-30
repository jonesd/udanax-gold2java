/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.integers;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerEdgeStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * A single instance of this class is cached.  To take advantage of this, a method
 * that uses IntegerEdgeSteppers should explicitly destroy at least one of them.
 */
public class IntegerEdgeStepper extends Stepper {

	protected boolean myEntering;
	protected int myIndex;
	protected int myCount;
	protected IntegerVarArray myEdges;
	protected static InstanceCache SomeEdgeSteppers;
/*
udanax-top.st:54373:
Stepper subclass: #IntegerEdgeStepper
	instanceVariableNames: '
		myEntering {BooleanVar}
		myIndex {UInt32}
		myCount {UInt32}
		myEdges {IntegerVarArray}'
	classVariableNames: 'SomeEdgeSteppers {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:54381:
IntegerEdgeStepper comment:
'A single instance of this class is cached.  To take advantage of this, a method
that uses IntegerEdgeSteppers should explicitly destroy at least one of them.'!
*/
/*
udanax-top.st:54384:
(IntegerEdgeStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:54450:
IntegerEdgeStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:54453:
(IntegerEdgeStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerEdgeStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (hasValue()) {
		return IntegerPos.make(edge());
	}
	else {
		return null;
	}
/*
udanax-top.st:54389:IntegerEdgeStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self hasValue ifTrue: [^IntegerPos make: self edge] ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return myIndex < myCount;
/*
udanax-top.st:54392:IntegerEdgeStepper methodsFor: 'operations'!
{BooleanVar INLINE} hasValue
	^myIndex < myCount!
*/
}
public void step() {
	myEntering = ! myEntering;
	myIndex = myIndex + 1;
/*
udanax-top.st:54395:IntegerEdgeStepper methodsFor: 'operations'!
{void INLINE} step
	myEntering _ myEntering not.
	myIndex _ myIndex + 1!
*/
}
/**
 * the current transition
 */
public int edge() {
	if (myIndex >= myCount) {
		IntegerEdgeStepper.outOfBounds();
	}
	return myEdges.integerVarAt(myIndex);
/*
udanax-top.st:54401:IntegerEdgeStepper methodsFor: 'edge accessing'!
{IntegerVar INLINE} edge
	"the current transition"
	(myIndex >= myCount) ifTrue: [ IntegerEdgeStepper outOfBounds ].
	^myEdges integerVarAt: myIndex!
*/
}
/**
 * whether the current transition is entering or leaving the set
 */
public boolean isEntering() {
	return myEntering;
/*
udanax-top.st:54406:IntegerEdgeStepper methodsFor: 'edge accessing'!
{BooleanVar INLINE} isEntering
	"whether the current transition is entering or leaving the set"
	^myEntering!
*/
}
public IntegerEdgeStepper(boolean entering, int count, IntegerVarArray edges) {
	super();
	myEntering = entering;
	myIndex = 0;
	myCount = count;
	myEdges = edges;
/*
udanax-top.st:54412:IntegerEdgeStepper methodsFor: 'protected: create'!
create: entering {BooleanVar} with: count {UInt32} with: edges {IntegerVarArray}
	super create.
	myEntering _ entering.
	myIndex _ Int32Zero.
	myCount _ count.
	myEdges _ edges!
*/
}
public IntegerEdgeStepper(boolean entering, int index, int count, IntegerVarArray edges) {
	super();
	myEntering = entering;
	myIndex = index;
	myCount = count;
	myEdges = edges;
/*
udanax-top.st:54419:IntegerEdgeStepper methodsFor: 'protected: create'!
create: entering {BooleanVar} with: index {UInt32} with: count {UInt32} with: edges {IntegerVarArray}
	super create.
	myEntering _ entering.
	myIndex _ index.
	myCount _ count.
	myEdges _ edges!
*/
}
public void destroy() {
	if ( ! (SomeEdgeSteppers.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:54428:IntegerEdgeStepper methodsFor: 'destroy'!
{void} destroy
	(SomeEdgeSteppers store: self) ifFalse:
		[super destroy]!
*/
}
public Stepper copy() {
	return new IntegerEdgeStepper(myEntering, myIndex, myCount, myEdges);
/*
udanax-top.st:54434:IntegerEdgeStepper methodsFor: 'create'!
{Stepper} copy
	^IntegerEdgeStepper create: myEntering with: myIndex with: myCount with: myEdges!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	if (hasValue()) {
		if (isEntering()) {
			oo.print("entering ");
		}
		else {
			oo.print("leaving ");
		}
		oo.print(edge());
	}
	oo.print(")");
/*
udanax-top.st:54439:IntegerEdgeStepper methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '('.
	self hasValue ifTrue:
		[self isEntering ifTrue: 
			[oo << 'entering ']
		ifFalse: 
			[oo << 'leaving '].
		oo << self edge].
	oo << ')'.!
*/
}
public static void outOfBounds() {
	throw new AboraRuntimeException(AboraRuntimeException.OUT_OF_BOUNDS);
/*
udanax-top.st:54458:IntegerEdgeStepper class methodsFor: 'errors'!
{void} outOfBounds
	self BLAST: #OutOfBounds!
*/
}
public static IntegerEdgeStepper make(boolean entering, int count, IntegerVarArray edges) {
	Heaper result;
	result = SomeEdgeSteppers.fetch();
	if (result == null) {
		return new IntegerEdgeStepper(entering, count, edges);
	}
	else {
		return 
		/* TODO newBecome */
		new IntegerEdgeStepper(entering, count, edges);
	}
/*
udanax-top.st:54463:IntegerEdgeStepper class methodsFor: 'create'!
make: entering {BooleanVar} with: count {UInt32} with: edges {IntegerVarArray}
	| result {Heaper} |
	result := SomeEdgeSteppers fetch.
	result == NULL ifTrue: [
		^ self create: entering with: count with: edges]
	ifFalse: [
		^ (self new.Become: result) create: entering with: count with: edges]!
*/
}
public static void initTimeNonInherited() {
	SomeEdgeSteppers = InstanceCache.make(2);
/*
udanax-top.st:54473:IntegerEdgeStepper class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeEdgeSteppers := InstanceCache make: 2!
*/
}
public static void linkTimeNonInherited() {
	SomeEdgeSteppers = null;
/*
udanax-top.st:54476:IntegerEdgeStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeEdgeSteppers := NULL!
*/
}
public IntegerEdgeStepper() {
/*

Generated during transformation
*/
}
public IntegerEdgeStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
