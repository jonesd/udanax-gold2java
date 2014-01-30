/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.id;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.id.IDSimpleStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class IDSimpleStepper extends Stepper {

	protected IDRegion myRegion;
	protected Stepper myBackends;
	protected Stepper myIDs;
	protected IDRegion myValue;
	protected IDRegion myInexplicit;
/*
udanax-top.st:54245:
Stepper subclass: #IDSimpleStepper
	instanceVariableNames: '
		myRegion {IDRegion}
		myBackends {Stepper | NULL of: Sequence}
		myIDs {Stepper | NULL of: (XnRegion of: Integer)}
		myValue {IDRegion | NULL}
		myInexplicit {IDRegion | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-id'!
*/
/*
udanax-top.st:54254:
(IDSimpleStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IDSimpleStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper copy() {
	return new IDSimpleStepper(myRegion, myBackends.copy(), myIDs.copy(), myInexplicit);
/*
udanax-top.st:54259:IDSimpleStepper methodsFor: 'create'!
{Stepper} copy
	
	^IDSimpleStepper create: myRegion with: myBackends copy with: myIDs copy with: myInexplicit!
*/
}
public IDSimpleStepper(IDRegion region) {
	super();
	myRegion = region;
	myBackends = region.explicitBackends().stepper();
	if (myBackends.hasValue()) {
		myIDs = (region.iDNumbersFrom(((Sequence) myBackends.fetch()))).simpleRegions();
	}
	else {
		myIDs = null;
		myBackends = null;
	}
	myValue = null;
	myInexplicit = region.fetchInexplicit();
/*
udanax-top.st:54263:IDSimpleStepper methodsFor: 'create'!
create: region {IDRegion}
	super create.
	myRegion := region.
	myBackends := region explicitBackends stepper.
	myBackends hasValue
		ifTrue: [myIDs := (region iDNumbersFrom: (myBackends fetch cast: Sequence)) simpleRegions]
		ifFalse: [myIDs := NULL. myBackends := NULL].
	myValue := NULL.
	myInexplicit := region fetchInexplicit!
*/
}
public IDSimpleStepper(IDRegion region, Stepper backends, Stepper iDs, IDRegion inexplicit) {
	super();
	myRegion = region;
	myBackends = backends;
	myIDs = iDs;
	myValue = null;
	myInexplicit = inexplicit;
/*
udanax-top.st:54274:IDSimpleStepper methodsFor: 'create'!
create: region {IDRegion} with: backends {Stepper of: Sequence} with: iDs {Stepper of: XnRegion} with: inexplicit {IDRegion | NULL}
	super create.
	myRegion := region.
	myBackends := backends.
	myIDs := iDs.
	myValue := NULL.
	myInexplicit := inexplicit.!
*/
}
public Heaper fetch() {
	if (myInexplicit != null) {
		return myInexplicit;
	}
	if (myValue == null && (myBackends != null)) {
		myValue = ((IDSpace) myRegion.coordinateSpace()).oldIDs(((Sequence) myBackends.fetch()), ((IntegerRegion) myIDs.fetch()));
	}
	return myValue;
/*
udanax-top.st:54285:IDSimpleStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	myInexplicit ~~ NULL ifTrue:
		[^myInexplicit].
	(myValue == NULL and: [myBackends ~~ NULL]) ifTrue:
		[myValue := (myRegion coordinateSpace cast: IDSpace)
			oldIDs: (myBackends fetch cast: Sequence)
			with: (myIDs fetch cast: IntegerRegion)].
	^myValue!
*/
}
public boolean hasValue() {
	return myInexplicit != null || (myBackends != null);
/*
udanax-top.st:54295:IDSimpleStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myInexplicit ~~ NULL or: [myBackends ~~ NULL]!
*/
}
public void step() {
	if (myInexplicit != null) {
		myInexplicit = null;
	}
	else {
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
				myIDs = (myRegion.iDNumbersFrom(((Sequence) myBackends.fetch()))).simpleRegions();
			}
		}
	}
/*
udanax-top.st:54299:IDSimpleStepper methodsFor: 'operations'!
{void} step
	myInexplicit ~~ NULL ifTrue:
		[myInexplicit := NULL]
	ifFalse: [myBackends ~~ NULL ifTrue:
		[myValue := NULL.
		myIDs step.
		myIDs hasValue ifFalse:
			[myBackends step.
			myBackends hasValue ifFalse:
				[myBackends := NULL.
				myIDs := NULL.
				^VOID].
			myIDs := (myRegion iDNumbersFrom: (myBackends fetch cast: Sequence)) simpleRegions]]]!
*/
}
public IDSimpleStepper() {
/*

Generated during transformation
*/
}
public IDSimpleStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
