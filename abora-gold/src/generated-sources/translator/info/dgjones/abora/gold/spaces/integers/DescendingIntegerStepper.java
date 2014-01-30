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
import info.dgjones.abora.gold.spaces.integers.DescendingIntegerStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class DescendingIntegerStepper extends Stepper {

	protected IntegerVarArray myEdges;
	protected int myIndex;
	protected int myPosition;
/*
udanax-top.st:53412:
Stepper subclass: #DescendingIntegerStepper
	instanceVariableNames: '
		myEdges {IntegerVarArray}
		myIndex {Int32}
		myPosition {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:53419:
(DescendingIntegerStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:53459:
DescendingIntegerStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:53462:
(DescendingIntegerStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DescendingIntegerStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public DescendingIntegerStepper(IntegerVarArray edges, int count) {
	super();
	myEdges = edges;
	myIndex = count - 2;
	if (myIndex >= -1) {
		myPosition = (myEdges.integerVarAt(myIndex + 1)) - 1;
	}
	else {
		myPosition = 0;
	}
/*
udanax-top.st:53424:DescendingIntegerStepper methodsFor: 'protected: create'!
create: edges {IntegerVarArray} with: count {UInt32}
	super create.
	myEdges _ edges.
	myIndex _ count - 2.
	myIndex >= -1
		ifTrue: [myPosition _ (myEdges integerVarAt: myIndex + 1) - 1]
		ifFalse: [myPosition _ IntegerVar0]!
*/
}
public DescendingIntegerStepper(IntegerVarArray edges, int index, int position) {
	super();
	myEdges = edges;
	myIndex = index;
	myPosition = position;
/*
udanax-top.st:53432:DescendingIntegerStepper methodsFor: 'protected: create'!
create: edges {IntegerVarArray} with: index {Int32} with: position {IntegerVar}
	super create.
	myEdges _ edges.
	myIndex _ index.
	myPosition _ position!
*/
}
public Stepper copy() {
	return new DescendingIntegerStepper(myEdges, myIndex, myPosition);
/*
udanax-top.st:53440:DescendingIntegerStepper methodsFor: 'creation'!
{Stepper} copy
	^DescendingIntegerStepper create: myEdges with: myIndex with: myPosition!
*/
}
public Heaper fetch() {
	if (hasValue()) {
		return IntegerPos.make(myPosition);
	}
	else {
		return null;
	}
/*
udanax-top.st:53445:DescendingIntegerStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	self hasValue ifTrue: [^myPosition integer] ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return myIndex >= -1;
/*
udanax-top.st:53448:DescendingIntegerStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^myIndex >= -1!
*/
}
public void step() {
	myPosition = myPosition - 1;
	if (myIndex >= 0 && (myPosition < (myEdges.integerVarAt(myIndex)))) {
		myIndex = myIndex - 2;
		if (myIndex >= -1) {
			myPosition = (myEdges.integerVarAt(myIndex + 1)) - 1;
		}
	}
/*
udanax-top.st:53451:DescendingIntegerStepper methodsFor: 'accessing'!
{void} step
	myPosition _ myPosition - 1.
	(myIndex >= Int32Zero and: [myPosition < (myEdges integerVarAt: myIndex)]) ifTrue:
		[myIndex _ myIndex - 2.
		myIndex >= -1 ifTrue:
			[myPosition _ (myEdges integerVarAt: myIndex + 1) - 1]]!
*/
}
public static Stepper make(IntegerVarArray edges, int count) {
	return new DescendingIntegerStepper(edges, count);
/*
udanax-top.st:53467:DescendingIntegerStepper class methodsFor: 'creation'!
{Stepper} make: edges {IntegerVarArray} with: count {UInt32}
	^ self create: edges with: count!
*/
}
public DescendingIntegerStepper() {
/*

Generated during transformation
*/
}
public DescendingIntegerStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
