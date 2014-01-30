/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.be.ents.MergeBundlesStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A Stepper for doing a merge-sort like ordered interleaving of two other steppers.  It is
 * assumed that the other two steppers are constructed so that their values are also produced
 * in order according to the same OrderSpec.  A tree of these operates much like a heap as
 * found in heapsort.
 */
public class MergeBundlesStepper extends Stepper {

	protected Stepper myA;
	protected Stepper myB;
	protected OrderSpec myOrder;
	protected FeBundle myValue;
/*
udanax-top.st:54613:
Stepper subclass: #MergeBundlesStepper
	instanceVariableNames: '
		myA {Stepper of: FeBundle}
		myB {Stepper of: FeBundle}
		myOrder {OrderSpec}
		myValue {FeBundle | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:54621:
MergeBundlesStepper comment:
'A Stepper for doing a merge-sort like ordered interleaving of two other steppers.  It is assumed that the other two steppers are constructed so that their values are also produced in order according to the same OrderSpec.  A tree of these operates much like a heap as found in heapsort.'!
*/
/*
udanax-top.st:54623:
(MergeBundlesStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:54698:
MergeBundlesStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:54701:
(MergeBundlesStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MergeBundlesStepper.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	if (myValue == null) {
		return Stepper.emptyStepper();
	}
	return new MergeBundlesStepper(myA.copy(), myB.copy(), myOrder, myValue);
/*
udanax-top.st:54628:MergeBundlesStepper methodsFor: 'operations'!
{Stepper} copy
	myValue == NULL ifTrue:
		[^Stepper emptyStepper].
	^MergeBundlesStepper
		create: myA copy
		with: myB copy
		with: myOrder
		with: myValue!
*/
}
public Heaper fetch() {
	return myValue;
/*
udanax-top.st:54638:MergeBundlesStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	
	^myValue!
*/
}
public boolean hasValue() {
	return myValue != null;
/*
udanax-top.st:54642:MergeBundlesStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	
	^myValue ~~ NULL!
*/
}
public void step() {
	FeBundle a;
	FeBundle b;
	a = (FeBundle) myA.fetch();
	b = (FeBundle) myB.fetch();
	if (a == null) {
		myValue = b;
		if (b != null) {
			myB.step();
		}
		return ;
	}
	if (b == null) {
		myValue = a;
		myA.step();
		return ;
	}
	if (myOrder.preceeds(a.region(), b.region())) {
		myValue = a;
		myA.step();
	}
	else {
		myValue = b;
		myB.step();
	}
/*
udanax-top.st:54646:MergeBundlesStepper methodsFor: 'operations'!
{void} step
	| a {FeBundle} b {FeBundle} |
	a := myA fetch cast: FeBundle.
	b := myB fetch cast: FeBundle.
	a == NULL ifTrue: 
		[myValue := b.
		b ~~ NULL ifTrue: [myB step].
		^VOID].
	b == NULL ifTrue: 
		[myValue := a.
		myA step.
		^VOID].
	(myOrder preceeds: a region with: b region)
		ifTrue: 
			[myValue := a.
			myA step]
		ifFalse: 
			[myValue := b.
			myB step]!
*/
}
public MergeBundlesStepper(Stepper a, Stepper b, OrderSpec order, FeBundle value) {
	super();
	myA = a;
	myB = b;
	myOrder = order;
	myValue = value;
	if (value == null) {
		step();
	}
/*
udanax-top.st:54668:MergeBundlesStepper methodsFor: 'private: creation'!
create: a {Stepper of: Position} 
	with: b {Stepper of: Position} 
	with: order {OrderSpec} 
	with: value {FeBundle | NULL}
	
	super create.
	myA := a.
	myB := b.
	myOrder := order.
	myValue := value.
	value == NULL ifTrue:
		[self step]!
*/
}
public MergeBundlesStepper(Rcvr receiver) {
	super(receiver);
	myA = (Stepper) receiver.receiveHeaper();
	myB = (Stepper) receiver.receiveHeaper();
	myOrder = (OrderSpec) receiver.receiveHeaper();
	myValue = (FeBundle) receiver.receiveHeaper();
/*
udanax-top.st:54683:MergeBundlesStepper methodsFor: 'generated:'!
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
udanax-top.st:54690:MergeBundlesStepper methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myA.
	xmtr sendHeaper: myB.
	xmtr sendHeaper: myOrder.
	xmtr sendHeaper: myValue.!
*/
}
public static Stepper make(Stepper a, Stepper b, OrderSpec order) {
	if ( ! (a.hasValue())) {
		return b;
	}
	if ( ! (b.hasValue())) {
		return a;
	}
	return new MergeBundlesStepper(a, b, order, null);
/*
udanax-top.st:54706:MergeBundlesStepper class methodsFor: 'creation'!
{Stepper} make: a {Stepper of: FeBundle} 
	with: b {Stepper of: FeBundle} 
	with: order {OrderSpec}
	
	a hasValue ifFalse: [^b].
	b hasValue ifFalse: [^a].
	^self create: a with: b with: order with: NULL!
*/
}
public MergeBundlesStepper() {
/*

Generated during transformation
*/
}
}
