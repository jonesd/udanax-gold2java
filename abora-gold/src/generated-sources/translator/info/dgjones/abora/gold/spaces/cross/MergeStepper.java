/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.cross;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.cross.MergeStepper;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A Stepper for doing a merge-sort like ordered interleaving of two other steppers.  It is
 * assumed that the other two steppers are constructed so that their values are also produced
 * in order according to the same OrderSpec.  A tree of these operates much like a heap as
 * found in heapsort.
 */
public class MergeStepper extends Stepper {

	protected Stepper myA;
	protected Stepper myB;
	protected OrderSpec myOrder;
	protected Position myValue;
/*
udanax-top.st:54714:
Stepper subclass: #MergeStepper
	instanceVariableNames: '
		myA {Stepper of: Position}
		myB {Stepper of: Position}
		myOrder {OrderSpec}
		myValue {Position | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Cross'!
*/
/*
udanax-top.st:54722:
MergeStepper comment:
'A Stepper for doing a merge-sort like ordered interleaving of two other steppers.  It is assumed that the other two steppers are constructed so that their values are also produced in order according to the same OrderSpec.  A tree of these operates much like a heap as found in heapsort.'!
*/
/*
udanax-top.st:54724:
(MergeStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:54806:
MergeStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:54809:
(MergeStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MergeStepper.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	if (myValue == null) {
		return Stepper.emptyStepper();
	}
	return new MergeStepper(myA.copy(), myB.copy(), myOrder, myValue);
/*
udanax-top.st:54729:MergeStepper methodsFor: 'operations'!
{Stepper} copy
	myValue == NULL ifTrue:
		[^Stepper emptyStepper].
	^MergeStepper
		create: myA copy
		with: myB copy
		with: myOrder
		with: myValue!
*/
}
public Heaper fetch() {
	return myValue;
/*
udanax-top.st:54739:MergeStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	
	^myValue!
*/
}
public boolean hasValue() {
	return myValue != null;
/*
udanax-top.st:54743:MergeStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	
	^myValue ~~ NULL!
*/
}
public void step() {
	Position a;
	Position b;
	a = (Position) myA.fetch();
	b = (Position) myB.fetch();
	if (a == null) {
		if (b == null) {
			myValue = null;
		}
		else {
			myValue = b;
			myB.step();
		}
	}
	else {
		if (b == null) {
			myValue = a;
			myA.step();
		}
		else {
			if (myOrder.follows(a, b)) {
				myValue = b;
				myB.step();
				if (a.isEqual(b)) {
					myA.step();
				}
			}
			else {
				myValue = a;
				myA.step();
				if (a.isEqual(b)) {
					myB.step();
				}
			}
		}
	}
/*
udanax-top.st:54747:MergeStepper methodsFor: 'operations'!
{void} step
	
	| a {Position} b {Position} |
	a := myA fetch cast: Position.
	b := myB fetch cast: Position.
	a == NULL ifTrue:
		[b == NULL ifTrue:
			[myValue := NULL]
		ifFalse:
			[myValue := b.
			myB step]]
	ifFalse:
		[b == NULL ifTrue:
			[myValue := a.
			myA step]
		ifFalse:
			[(myOrder follows: a with: b) ifTrue:
				[myValue := b.
				myB step.
				(a isEqual: b) ifTrue:
					[myA step]]
			ifFalse:
				[myValue := a.
				myA step.
				(a isEqual: b) ifTrue:
					[myB step]]]]!
*/
}
public MergeStepper(Stepper a, Stepper b, OrderSpec order, Position value) {
	super();
	myA = a;
	myB = b;
	myOrder = order;
	myValue = value;
	if (value == null) {
		step();
	}
/*
udanax-top.st:54776:MergeStepper methodsFor: 'private: creation'!
create: a {Stepper of: Position} 
	with: b {Stepper of: Position} 
	with: order {OrderSpec} 
	with: value {Position | NULL}
	
	super create.
	myA := a.
	myB := b.
	myOrder := order.
	myValue := value.
	value == NULL ifTrue:
		[self step]!
*/
}
public MergeStepper(Rcvr receiver) {
	super(receiver);
	myA = (Stepper) receiver.receiveHeaper();
	myB = (Stepper) receiver.receiveHeaper();
	myOrder = (OrderSpec) receiver.receiveHeaper();
	myValue = (Position) receiver.receiveHeaper();
/*
udanax-top.st:54791:MergeStepper methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myA _ receiver receiveHeaper.
	myB _ receiver receiveHeaper.
	myOrder _ receiver receiveHeaper.
	myValue _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myA);
	xmtr.sendHeaper(myB);
	xmtr.sendHeaper(myOrder);
	xmtr.sendHeaper(myValue);
/*
udanax-top.st:54798:MergeStepper methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myA.
	xmtr sendHeaper: myB.
	xmtr sendHeaper: myOrder.
	xmtr sendHeaper: myValue.!
*/
}
public static Stepper make(Stepper a, Stepper b, OrderSpec order) {
	return new MergeStepper(a, b, order, null);
/*
udanax-top.st:54814:MergeStepper class methodsFor: 'pseudoconstructors'!
{Stepper} make: a {Stepper of: Position} 
	with: b {Stepper of: Position} 
	with: order {OrderSpec}
	
	^self create: a with: b with: order with: NULL!
*/
}
public MergeStepper() {
/*

Generated during transformation
*/
}
}
