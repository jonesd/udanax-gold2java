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
import info.dgjones.abora.gold.primtab.PrimPtrTableStepper;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Stepper over map from integers to strong or wimpy pointers
 */
public class PrimPtrTableStepper extends Stepper {

	protected PtrArray myPtrs;
	protected IntegerVarArray myIndices;
	protected int myIndex;
/*
udanax-top.st:54925:
Stepper subclass: #PrimPtrTableStepper
	instanceVariableNames: '
		myPtrs {PtrArray}
		myIndices {IntegerVarArray}
		myIndex {Int4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:54932:
PrimPtrTableStepper comment:
'Stepper over map from integers to strong or wimpy pointers'!
*/
/*
udanax-top.st:54934:
(PrimPtrTableStepper getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimPtrTableStepper -/
friend class PrimPtrTable;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPtrTableStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public PrimPtrTableStepper(IntegerVarArray from, PtrArray to, int index) {
	super();
	Heaper tmp;
	myIndices = from;
	myPtrs = to;
	myIndex = index;
	while (myIndex < myPtrs.count() && ((tmp = myPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:54942:PrimPtrTableStepper methodsFor: 'protected: create'!
create: from {IntegerVarArray} with: to {PtrArray} with: index {Int32}
	| tmp {Heaper wimpy} |
	super create.
	myIndices := from.
	myPtrs := to.
	myIndex := index.
	[myIndex < myPtrs count and: [(tmp _ myPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public Heaper fetch() {
	if (myIndex < myPtrs.count()) {
		return myPtrs.fetch(myIndex);
	}
	else {
		return null;
	}
/*
udanax-top.st:54953:PrimPtrTableStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	myIndex < myPtrs count ifTrue: [ ^ myPtrs fetch: myIndex ]
	ifFalse: [ ^ NULL ]!
*/
}
public boolean hasValue() {
	return myIndex < myPtrs.count();
/*
udanax-top.st:54957:PrimPtrTableStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^ myIndex < myPtrs count!
*/
}
public int index() {
	if (myIndex < myIndices.count()) {
		return myIndices.integerVarAt(myIndex);
	}
	throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
/*
udanax-top.st:54960:PrimPtrTableStepper methodsFor: 'accessing'!
{IntegerVar} index
	myIndex < myIndices count ifTrue: [^ myIndices integerVarAt: myIndex].
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
udanax-top.st:54965:PrimPtrTableStepper methodsFor: 'accessing'!
{void} step
	|tmp {Heaper wimpy} |
	myIndex := myIndex + 1.
	[myIndex < myPtrs count and: [(tmp _ myPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public Stepper copy() {
	return new PrimPtrTableStepper(myIndices, myPtrs, myIndex);
/*
udanax-top.st:54973:PrimPtrTableStepper methodsFor: 'create'!
{Stepper} copy
	^ PrimPtrTableStepper create: myIndices with: myPtrs with: myIndex!
*/
}
public PrimPtrTableStepper() {
/*

Generated during transformation
*/
}
public PrimPtrTableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
