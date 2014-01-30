/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.id;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.id.IDStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class IDStepper extends Stepper {

	protected IDRegion myRegion;
	protected Stepper myBackends;
	protected Stepper myIDs;
	protected ID myValue;
/*
udanax-top.st:54313:
Stepper subclass: #IDStepper
	instanceVariableNames: '
		myRegion {IDRegion}
		myBackends {Stepper | NULL of: Sequence}
		myIDs {Stepper | NULL of: IntegerPos}
		myValue {ID | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-id'!
*/
/*
udanax-top.st:54321:
(IDStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IDStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return new IDStepper(myRegion, myBackends.copy(), myIDs.copy());
/*
udanax-top.st:54326:IDStepper methodsFor: 'create'!
{Stepper} copy
	
	^IDStepper create: myRegion with: myBackends copy with: myIDs copy!
*/
}
public IDStepper(IDRegion region) {
	super();
	myRegion = region;
	myBackends = region.backends().stepper();
	if (myBackends.hasValue()) {
		myIDs = (region.iDNumbersFrom(((Sequence) myBackends.fetch()))).stepper();
	}
	else {
		myIDs = null;
		myBackends = null;
	}
	myValue = null;
/*
udanax-top.st:54330:IDStepper methodsFor: 'create'!
create: region {IDRegion}
	super create.
	myRegion := region.
	myBackends := region backends stepper.
	myBackends hasValue
		ifTrue: [myIDs := (region iDNumbersFrom: (myBackends fetch cast: Sequence)) stepper]
		ifFalse: [myIDs := NULL. myBackends := NULL].
	myValue := NULL.!
*/
}
public IDStepper(IDRegion region, Stepper backends, Stepper iDs) {
	super();
	myRegion = region;
	myBackends = backends;
	myIDs = iDs;
	myValue = null;
/*
udanax-top.st:54340:IDStepper methodsFor: 'create'!
create: region {IDRegion} with: backends {Stepper of: Sequence} with: iDs {Stepper of: IntegerPos}
	super create.
	myRegion := region.
	myBackends := backends.
	myIDs := iDs.
	myValue := NULL.!
*/
}
public Heaper fetch() {
	if (myValue == null && (myBackends != null)) {
		myValue = ID.usingx(myRegion.fetchSpace(), ((Sequence) myBackends.fetch()), ((IntegerPos) myIDs.fetch()).asIntegerVar());
	}
	return myValue;
/*
udanax-top.st:54349:IDStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	(myValue == NULL and: [myBackends ~~ NULL]) ifTrue:
		[myValue := ID usingx: myRegion fetchSpace
			with: (myBackends fetch cast: Sequence)
			with: (myIDs fetch cast: IntegerPos) asIntegerVar].
	^myValue!
*/
}
public boolean hasValue() {
	return myBackends != null;
/*
udanax-top.st:54357:IDStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myBackends ~~ NULL!
*/
}
public void step() {
	if (myBackends != null) {
		myValue = null;
		myIDs.step();
		if ( ! (myIDs.hasValue())) {
			myBackends.step();
			if ( ! (myBackends.hasValue())) {
				myBackends = null;
				myIDs = null;
				return ;
			}
			myIDs = (myRegion.iDNumbersFrom(((Sequence) myBackends.fetch()))).stepper();
		}
	}
/*
udanax-top.st:54361:IDStepper methodsFor: 'operations'!
{void} step
	myBackends ~~ NULL ifTrue:
		[myValue := NULL.
		myIDs step.
		myIDs hasValue ifFalse:
			[myBackends step.
			myBackends hasValue ifFalse:
				[myBackends := NULL.
				myIDs := NULL.
				^VOID].
			myIDs := (myRegion iDNumbersFrom: (myBackends fetch cast: Sequence)) stepper]]!
*/
}
public IDStepper() {
/*

Generated during transformation
*/
}
public IDStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
