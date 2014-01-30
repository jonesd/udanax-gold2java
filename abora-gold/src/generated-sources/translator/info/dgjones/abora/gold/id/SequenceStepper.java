/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.id;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.SequenceStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SequenceStepper extends Stepper {

	protected int myIndex;
	protected PtrArray myTransitions;
	protected int myTransitionsCount;
/*
udanax-top.st:55120:
Stepper subclass: #SequenceStepper
	instanceVariableNames: '
		myIndex {Int32}
		myTransitions {PtrArray of: SequenceEdge}
		myTransitionsCount {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-id'!
*/
/*
udanax-top.st:55127:
(SequenceStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SequenceStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (myIndex < myTransitionsCount) {
		return ((SequenceEdge) (myTransitions.fetch(myIndex))).sequence();
	}
	else {
		return null;
	}
/*
udanax-top.st:55132:SequenceStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	myIndex < myTransitionsCount ifTrue:
		[^((myTransitions fetch: myIndex) cast: SequenceEdge) sequence]
	ifFalse:
		[^NULL]!
*/
}
public boolean hasValue() {
	return myIndex < myTransitionsCount;
/*
udanax-top.st:55139:SequenceStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myIndex < myTransitionsCount!
*/
}
public void step() {
	myIndex = myIndex + 2;
/*
udanax-top.st:55142:SequenceStepper methodsFor: 'operations'!
{void} step
	myIndex := myIndex + 2!
*/
}
public Stepper copy() {
	return new SequenceStepper(myIndex, myTransitions, myTransitionsCount);
/*
udanax-top.st:55147:SequenceStepper methodsFor: 'create'!
{Stepper} copy
	
	^SequenceStepper create: myIndex with: myTransitions with: myTransitionsCount!
*/
}
public SequenceStepper(PtrArray transitions, int count) {
	super();
	myIndex = 0;
	myTransitions = transitions;
	myTransitionsCount = count;
/*
udanax-top.st:55151:SequenceStepper methodsFor: 'create'!
create: transitions {PtrArray of: IDEdge} with: count {Int32}
	super create.
	myIndex := Int32Zero.
	myTransitions := transitions.
	myTransitionsCount := count.!
*/
}
public SequenceStepper(int index, PtrArray transitions, int count) {
	super();
	myIndex = index;
	myTransitions = transitions;
	myTransitionsCount = count;
/*
udanax-top.st:55158:SequenceStepper methodsFor: 'create'!
create: index {Int32} with: transitions {PtrArray of: IDEdge} with: count {Int32}
	super create.
	myIndex := index.
	myTransitions := transitions.
	myTransitionsCount := count.!
*/
}
public SequenceStepper() {
/*

Generated during transformation
*/
}
public SequenceStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
