/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.primtab.PrimSetStepper;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class PrimSetStepper extends Stepper {

	protected PtrArray myPtrs;
	protected int myIndex;
	protected static InstanceCache SomeSteppers;
/*
udanax-top.st:54976:
Stepper subclass: #PrimSetStepper
	instanceVariableNames: '
		myPtrs {PtrArray}
		myIndex {Int4}'
	classVariableNames: 'SomeSteppers {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:54982:
(PrimSetStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:55036:
PrimSetStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55039:
(PrimSetStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimSetStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new PrimSetStepper(myPtrs, 0);
	}
	else {
		return 
		/* TODO newBecome */
		new PrimSetStepper(myPtrs, 0);
	}
/*
udanax-top.st:54987:PrimSetStepper methodsFor: 'create'!
{Stepper} copy
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^ PrimSetStepper create: myPtrs with: Int32Zero]
		ifFalse: [^ (PrimSetStepper new.Become: result) create: myPtrs with: Int32Zero]!
*/
}
public PrimSetStepper(PtrArray array, int index) {
	super();
	Heaper tmp;
	myPtrs = array;
	myIndex = index;
	while (myIndex < myPtrs.count() && ((tmp = myPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:54994:PrimSetStepper methodsFor: 'create'!
create: array {PtrArray} with: index {Int32}
	| tmp {Heaper wimpy} |
	super create.
	myPtrs := array.
	myIndex := index.
	[myIndex < myPtrs count and: [(tmp _ myPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public void destroy() {
	if ( ! (SomeSteppers.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:55002:PrimSetStepper methodsFor: 'create'!
{void} destroy
	(SomeSteppers store: self) ifFalse:
		[super destroy]!
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
udanax-top.st:55008:PrimSetStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	myIndex < myPtrs count
		ifTrue: [ ^ myPtrs fetch: myIndex ]
		ifFalse: [ ^ NULL ]!
*/
}
public boolean hasValue() {
	return myIndex < myPtrs.count();
/*
udanax-top.st:55013:PrimSetStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^ myIndex < myPtrs count!
*/
}
public void step() {
	Heaper tmp;
	myIndex = myIndex + 1;
	while (myIndex < myPtrs.count() && ((tmp = myPtrs.fetch(myIndex)) == null || (tmp == PrimRemovedObject.make()))) {
		myIndex = myIndex + 1;
	}
/*
udanax-top.st:55016:PrimSetStepper methodsFor: 'accessing'!
{void} step
	|tmp {Heaper wimpy} |
	myIndex := myIndex + 1.
	[myIndex < myPtrs count and: [(tmp _ myPtrs fetch: myIndex) == NULL or: [tmp == PrimRemovedObject make]]]
		whileTrue: [ myIndex := myIndex + 1 ].!
*/
}
public void printOn(PrintWriter oo) {
	boolean printedElem;
	oo.print("PrimSetStepper on {");
	printedElem = false;
	for (int i = 0; i < myPtrs.count(); i ++ ) {
		if ((myPtrs.fetch(i)) != null) {
			if (printedElem) {
				oo.print(", ");
			}
			oo.print((myPtrs.fetch(i)));
			printedElem = true;
		}
	}
	oo.print("}");
/*
udanax-top.st:55024:PrimSetStepper methodsFor: 'printint'!
{void} printOn: oo {ostream reference}
	| printedElem {BooleanVar} |
	oo << 'PrimSetStepper on {'.
	printedElem := false.
	Int32Zero almostTo: myPtrs count do: [:i {Int32} |
		(myPtrs fetch: i) ~~ NULL ifTrue: [
			printedElem ifTrue: [oo << ', '].
			oo << (myPtrs fetch: i).
			printedElem := true]].
	oo << '}'!
*/
}
public static Stepper make(PtrArray ptrs) {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new PrimSetStepper(ptrs, 0);
	}
	else {
		return 
		/* TODO newBecome */
		new PrimSetStepper(ptrs, 0);
	}
/*
udanax-top.st:55044:PrimSetStepper class methodsFor: 'create'!
{Stepper} make: ptrs {PtrArray}
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^ PrimSetStepper create: ptrs with: Int32Zero]
		ifFalse: [^ (PrimSetStepper new.Become: result) create: ptrs with: Int32Zero]!
*/
}
public static void initTimeNonInherited() {
	SomeSteppers = InstanceCache.make(8);
/*
udanax-top.st:55053:PrimSetStepper class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeSteppers := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeSteppers = null;
/*
udanax-top.st:55056:PrimSetStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeSteppers := NULL!
*/
}
public PrimSetStepper() {
/*

Generated during transformation
*/
}
public PrimSetStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
