/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTableStepper;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Stepper over map from pointers to integers
 */
public class PrimIndexTableStepper extends Stepper {

	protected PtrArray myPtrs;
	protected IntegerVarArray myIndices;
	protected int myIndex;
/*
udanax-top.st:54820:
Stepper subclass: #PrimIndexTableStepper
	instanceVariableNames: '
		myPtrs {PtrArray}
		myIndices {IntegerVarArray}
		myIndex {Int4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:54827:
PrimIndexTableStepper comment:
'Stepper over map from pointers to integers'!
*/
/*
udanax-top.st:54829:
(PrimIndexTableStepper getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimIndexTableStepper -/
friend class PrimIndexTable;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIndexTableStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (myIndex < myPtrs.count()) {
		return IntegerPos.make((myIndices.integerVarAt(myIndex)));
	}
	else {
		return null;
	}
/*
udanax-top.st:54837:PrimIndexTableStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	myIndex < myPtrs count ifTrue: [ ^ IntegerPos make: (myIndices integerVarAt: myIndex) ]
	ifFalse: [ ^ NULL ]!
*/
}
public boolean hasValue() {
	return myIndex < myPtrs.count();
/*
udanax-top.st:54841:PrimIndexTableStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^ myIndex < myPtrs count!
*/
}
/**
 * This does not necessarily return a Position
 */
public Heaper key() {
	if (myIndex < myIndices.count()) {
		return myPtrs.fetch(myIndex);
	}
	throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
/*
udanax-top.st:54844:PrimIndexTableStepper methodsFor: 'accessing'!
{Heaper}  key
	"This does not necessarily return a Position"
	myIndex < myIndices count ifTrue: [^ myPtrs fetch: myIndex].
	Heaper BLAST: #EmptyStepper.
	^ NULL "Hush up the compiler"!
*/
}
public void step() {
	Heaper tmp;
	myIndex = myIndex + 1;
	while (myIndex < myPtrs.count() && ((tmp = myPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:54850:PrimIndexTableStepper methodsFor: 'accessing'!
{void} step
	|tmp {Heaper wimpy} |
	myIndex := myIndex + 1.
	[myIndex < myPtrs count and: [(tmp _ myPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public int value() {
	if (myIndex < myPtrs.count()) {
		return myIndices.integerVarAt(myIndex);
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
	}
/*
udanax-top.st:54856:PrimIndexTableStepper methodsFor: 'accessing'!
{IntegerVar} value
	myIndex < myPtrs count ifTrue: [ ^ myIndices integerVarAt: myIndex]
	ifFalse: [ Heaper BLAST: #EmptyStepper.
			^ NULL ]!
*/
}
public PrimIndexTableStepper(PtrArray from, IntegerVarArray to, int index) {
	super();
	Heaper tmp;
	myPtrs = from;
	myIndices = to;
	myIndex = index;
	while (myIndex < myPtrs.count() && ((tmp = myPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:54863:PrimIndexTableStepper methodsFor: 'protected: create'!
create: from {PtrArray} with: to {IntegerVarArray} with: index {Int32}
	| tmp {Heaper wimpy} |
	super create.
	myPtrs := from.
	myIndices := to.
	myIndex := index.
	[myIndex < myPtrs count and: [(tmp _ myPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public Stepper copy() {
	return new PrimIndexTableStepper(myPtrs, myIndices, myIndex);
/*
udanax-top.st:54874:PrimIndexTableStepper methodsFor: 'create'!
{Stepper} copy
	^ PrimIndexTableStepper create: myPtrs with: myIndices with: myIndex!
*/
}
public PrimIndexTableStepper() {
/*

Generated during transformation
*/
}
public PrimIndexTableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
