/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTableStepper;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class PrimPtr2PtrTableStepper extends Stepper {

	protected PtrArray myFromPtrs;
	protected PtrArray myToPtrs;
	protected int myIndex;
/*
udanax-top.st:54877:
Stepper subclass: #PrimPtr2PtrTableStepper
	instanceVariableNames: '
		myFromPtrs {PtrArray}
		myToPtrs {PtrArray}
		myIndex {Int4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:54884:
(PrimPtr2PtrTableStepper getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimPtr2PtrTableStepper -/
friend class PrimPtr2PtrTable;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPtr2PtrTableStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return new PrimPtr2PtrTableStepper(myFromPtrs, myToPtrs, myIndex);
/*
udanax-top.st:54892:PrimPtr2PtrTableStepper methodsFor: 'create'!
{Stepper} copy
	^ PrimPtr2PtrTableStepper create: myFromPtrs with: myToPtrs with: myIndex!
*/
}
public Heaper fetch() {
	if (myIndex < myToPtrs.count()) {
		return myToPtrs.fetch(myIndex);
	}
	else {
		return null;
	}
/*
udanax-top.st:54897:PrimPtr2PtrTableStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	myIndex < myToPtrs count ifTrue: [ ^ myToPtrs fetch: myIndex ]
	ifFalse: [ ^ NULL ]!
*/
}
public boolean hasValue() {
	return myIndex < myToPtrs.count();
/*
udanax-top.st:54901:PrimPtr2PtrTableStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^ myIndex < myToPtrs count!
*/
}
public Heaper heaperKey() {
	if (myIndex < myFromPtrs.count()) {
		return myFromPtrs.fetch(myIndex);
	}
	else {
		return null;
	}
/*
udanax-top.st:54904:PrimPtr2PtrTableStepper methodsFor: 'accessing'!
{Heaper} heaperKey
	myIndex < myFromPtrs count ifTrue: [^ myFromPtrs fetch: myIndex]
	ifFalse: [ ^ NULL ]!
*/
}
public void step() {
	Heaper tmp;
	myIndex = myIndex + 1;
	while (myIndex < myToPtrs.count() && ((tmp = myToPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:54908:PrimPtr2PtrTableStepper methodsFor: 'accessing'!
{void} step
	|tmp {Heaper wimpy} |
	myIndex := myIndex + 1.
	[myIndex < myToPtrs count and: [(tmp _ myToPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public PrimPtr2PtrTableStepper(PtrArray from, PtrArray to, int index) {
	super();
	Heaper tmp;
	myFromPtrs = from;
	myToPtrs = to;
	myIndex = index;
	while (myIndex < myToPtrs.count() && ((tmp = myToPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:54916:PrimPtr2PtrTableStepper methodsFor: 'protected: create'!
create: from {PtrArray} with: to {PtrArray} with: index {Int32}
	| tmp {Heaper wimpy} |
	super create.
	myFromPtrs := from.
	myToPtrs := to.
	myIndex := index.
	[myIndex < myToPtrs count and: [(tmp _ myToPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public PrimPtr2PtrTableStepper() {
/*

Generated during transformation
*/
}
public PrimPtr2PtrTableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
