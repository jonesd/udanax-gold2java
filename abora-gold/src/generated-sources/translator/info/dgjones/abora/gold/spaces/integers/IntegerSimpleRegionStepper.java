/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.integers;

import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSimpleRegionStepper;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class IntegerSimpleRegionStepper extends Stepper {

	protected IntegerVarArray myEdges;
	protected int myIndex;
	protected int myCount;
	protected boolean isLeftBounded;
	protected IntegerRegion mySimple;
/*
udanax-top.st:54479:
Stepper subclass: #IntegerSimpleRegionStepper
	instanceVariableNames: '
		myEdges {IntegerVarArray}
		myIndex {UInt32}
		myCount {UInt32}
		isLeftBounded {BooleanVar}
		mySimple {IntegerRegion}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:54488:
(IntegerSimpleRegionStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IntegerSimpleRegionStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	return mySimple;
/*
udanax-top.st:54493:IntegerSimpleRegionStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	^mySimple!
*/
}
public boolean hasValue() {
	return mySimple != null;
/*
udanax-top.st:54496:IntegerSimpleRegionStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^mySimple ~~ NULL!
*/
}
public void step() {
	if (isLeftBounded) {
		myIndex = myIndex + 2;
	}
	else {
		myIndex = myIndex + 1;
	}
	isLeftBounded = true;
	if (myIndex < myCount) {
		if (myIndex < (myCount - 1)) {
			mySimple = IntegerRegion.make((myEdges.integerVarAt(myIndex)), (myEdges.integerVarAt(myIndex + 1)));
		}
		else {
			mySimple = IntegerRegion.after((myEdges.integerVarAt(myIndex)));
		}
	}
	else {
		mySimple = null;
	}
/*
udanax-top.st:54499:IntegerSimpleRegionStepper methodsFor: 'operations'!
{void} step
	isLeftBounded ifTrue: [myIndex _ myIndex + 2] ifFalse: [myIndex _ myIndex + 1].
	isLeftBounded _ true.
	myIndex < myCount
		ifTrue: [myIndex < (myCount - 1)
			ifTrue: [mySimple _ IntegerRegion make: (myEdges integerVarAt: myIndex)
				with: (myEdges integerVarAt: myIndex + 1)]
			ifFalse: [mySimple _ IntegerRegion after: (myEdges integerVarAt: myIndex)]]
		ifFalse: [mySimple _ NULL]!
*/
}
public IntegerSimpleRegionStepper(IntegerVarArray edges, int count, boolean leftBounded) {
	super();
	myEdges = edges;
	myIndex = 0;
	myCount = count;
	isLeftBounded = leftBounded;
	if (count == 0) {
		if (leftBounded) {
			mySimple = null;
		}
		else {
			mySimple = IntegerRegion.allIntegers();
		}
	}
	else {
		if ( ! leftBounded) {
			mySimple = IntegerRegion.before((edges.integerVarAt(0)));
		}
		else {
			if (count == 1) {
				mySimple = IntegerRegion.after((edges.integerVarAt(0)));
			}
			else {
				mySimple = IntegerRegion.make((edges.integerVarAt(0)), (edges.integerVarAt(1)));
			}
		}
	}
/*
udanax-top.st:54511:IntegerSimpleRegionStepper methodsFor: 'unprotected create'!
create: edges {IntegerVarArray} with: count {UInt32} with: leftBounded {BooleanVar}
	super create.
	myEdges _ edges.
	myIndex _ Int32Zero.
	myCount _ count.
	isLeftBounded _ leftBounded.  
	count == Int32Zero ifTrue:
		[leftBounded ifTrue: [mySimple _ NULL] ifFalse: [mySimple _ IntegerRegion allIntegers]]
	ifFalse: [leftBounded not ifTrue:
		[mySimple _ IntegerRegion before: (edges integerVarAt: Int32Zero)]
	ifFalse: [count = 1 ifTrue:
		[mySimple _ IntegerRegion after: (edges integerVarAt: Int32Zero)]
	ifFalse:
		[mySimple _ IntegerRegion make: (edges integerVarAt: Int32Zero) with: (edges integerVarAt: 1)]]]!
*/
}
public IntegerSimpleRegionStepper(IntegerVarArray edges, int index, int count, boolean leftBounded, IntegerRegion simple) {
	super();
	myEdges = edges;
	myIndex = index;
	myCount = count;
	isLeftBounded = leftBounded;
	mySimple = simple;
/*
udanax-top.st:54526:IntegerSimpleRegionStepper methodsFor: 'unprotected create'!
create: edges {IntegerVarArray} with: index {UInt32} with: count {UInt32} with: leftBounded {BooleanVar} with: simple {IntegerRegion}
	super create.
	myEdges _ edges.
	myIndex _ index.
	myCount _ count.
	isLeftBounded _ leftBounded.
	mySimple _ simple!
*/
}
public Stepper copy() {
	return new IntegerSimpleRegionStepper(myEdges, myIndex, myCount, isLeftBounded, mySimple);
/*
udanax-top.st:54536:IntegerSimpleRegionStepper methodsFor: 'create'!
{Stepper} copy
	^IntegerSimpleRegionStepper create: myEdges
		with: myIndex
		with: myCount
		with: isLeftBounded
		with: mySimple!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	if (hasValue()) {
		oo.print(fetch());
	}
	oo.print(")");
/*
udanax-top.st:54545:IntegerSimpleRegionStepper methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '('.
	self hasValue ifTrue: [oo << self fetch].
	oo << ')'!
*/
}
public IntegerSimpleRegionStepper() {
/*

Generated during transformation
*/
}
public IntegerSimpleRegionStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
