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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.AscendingIntegerStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class AscendingIntegerStepper extends Stepper {

	protected IntegerVarArray myEdges;
	protected int myIndex;
	protected int myCount;
	protected int myPosition;
	protected static InstanceCache SomeSteppers;
/*
udanax-top.st:52949:
Stepper subclass: #AscendingIntegerStepper
	instanceVariableNames: '
		myEdges {IntegerVarArray}
		myIndex {UInt32}
		myCount {UInt32}
		myPosition {IntegerVar}'
	classVariableNames: 'SomeSteppers {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Integers'!
*/
/*
udanax-top.st:52957:
(AscendingIntegerStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:53007:
AscendingIntegerStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:53010:
(AscendingIntegerStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AscendingIntegerStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public AscendingIntegerStepper(IntegerVarArray edges, int count) {
	super();
	myEdges = edges;
	myIndex = 1;
	myCount = count;
	if (myCount > 0) {
		myPosition = myEdges.integerVarAt(0);
	}
	else {
		myPosition = 0;
	}
/*
udanax-top.st:52962:AscendingIntegerStepper methodsFor: 'protected: creation'!
create: edges {IntegerVarArray} with: count {UInt32}
	super create.
	myEdges _ edges.
	myIndex _ 1.
	myCount _ count.
	myCount > Int32Zero
		ifTrue: [myPosition _ myEdges integerVarAt: Int32Zero]
		ifFalse: [myPosition _ IntegerVar0]!
*/
}
public AscendingIntegerStepper(IntegerVarArray edges, int index, int count, int position) {
	super();
	myEdges = edges;
	myIndex = index;
	myCount = count;
	myPosition = position;
/*
udanax-top.st:52972:AscendingIntegerStepper methodsFor: 'protected: creation'!
create: edges {IntegerVarArray} with: index {UInt32} with: count {UInt32} with: position {IntegerVar}
	super create.
	myEdges _ edges.
	myIndex _ index.
	myCount _ count.
	myPosition _ position!
*/
}
public Stepper copy() {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new AscendingIntegerStepper(myEdges, myIndex, myCount, myPosition);
	}
	else {
		return 
		/* TODO newBecome */
		new AscendingIntegerStepper(myEdges, myIndex, myCount, myPosition);
	}
/*
udanax-top.st:52981:AscendingIntegerStepper methodsFor: 'creation'!
{Stepper} copy
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^AscendingIntegerStepper create: myEdges with: myIndex with: myCount with: myPosition]
		ifFalse:[^(AscendingIntegerStepper new.Become: result) create: myEdges with: myIndex with: myCount with: myPosition]!
*/
}
public void destroy() {
	if ( ! (SomeSteppers.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:52988:AscendingIntegerStepper methodsFor: 'creation'!
{void} destroy
	(SomeSteppers store: self) ifFalse: [super destroy]!
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
udanax-top.st:52993:AscendingIntegerStepper methodsFor: 'accessing'!
{Heaper wimpy} fetch
	self hasValue ifTrue: [^myPosition integer] ifFalse: [^NULL]!
*/
}
public boolean hasValue() {
	return myIndex <= myCount;
/*
udanax-top.st:52996:AscendingIntegerStepper methodsFor: 'accessing'!
{BooleanVar} hasValue
	^myIndex <= myCount!
*/
}
public void step() {
	myPosition = myPosition + 1;
	if (myIndex < myCount && (myPosition == (myEdges.integerVarAt(myIndex)))) {
		myIndex = myIndex + 2;
		if (myIndex <= myCount) {
			myPosition = myEdges.integerVarAt(myIndex - 1);
		}
	}
/*
udanax-top.st:52999:AscendingIntegerStepper methodsFor: 'accessing'!
{void} step
	myPosition _ myPosition + 1.
	(myIndex < myCount and: [myPosition = (myEdges integerVarAt: myIndex)]) ifTrue:
		[myIndex _ myIndex + 2.
		myIndex <= myCount ifTrue:
			[myPosition _ myEdges integerVarAt: myIndex - 1]]!
*/
}
public static void initTimeNonInherited() {
	SomeSteppers = InstanceCache.make(16);
/*
udanax-top.st:53015:AscendingIntegerStepper class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeSteppers := InstanceCache make: 16!
*/
}
public static void linkTimeNonInherited() {
	SomeSteppers = null;
/*
udanax-top.st:53018:AscendingIntegerStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeSteppers := NULL!
*/
}
public static Stepper make(IntegerVarArray edges, int count) {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new AscendingIntegerStepper(edges, count);
	}
	else {
		return 
		/* TODO newBecome */
		new AscendingIntegerStepper(edges, count);
	}
/*
udanax-top.st:53023:AscendingIntegerStepper class methodsFor: 'creation'!
{Stepper} make: edges {IntegerVarArray} with: count {UInt32}
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^ self create: edges with: count]
		ifFalse: [^ (self new.Become: result) create: edges with: count]!
*/
}
public AscendingIntegerStepper() {
/*

Generated during transformation
*/
}
public AscendingIntegerStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
