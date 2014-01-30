/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.Pumpkin;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Pumpkin extends Abraham {

	protected static Abraham TheGreatPumpkin;
/*
udanax-top.st:10490:
Abraham subclass: #Pumpkin
	instanceVariableNames: ''
	classVariableNames: 'TheGreatPumpkin {Abraham} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:10494:
(Pumpkin getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #EQ; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:10523:
Pumpkin class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:10526:
(Pumpkin getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #EQ; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Pumpkin.class).setAttributes( new Set().add("LOCKED").add("COPY").add("EQ").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * This can only be implemented by classes which are shepherds.
 */
public void becomeStub() {
	/* Each subclass will have expressions of the form: 'new (this) MyStubClass()' */
	shouldNotImplement();
/*
udanax-top.st:10499:Pumpkin methodsFor: 'protected: protected'!
{void} becomeStub
	"This can only be implemented by classes which are shepherds."
	"Each subclass will have expressions of the form: 'new (this) MyStubClass()'"
	self shouldNotImplement!
*/
}
public Pumpkin(int hash) {
	super(hash);
/*
udanax-top.st:10507:Pumpkin methodsFor: 'creation'!
create: hash {UInt32}
	super create: hash!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:10512:Pumpkin methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public Pumpkin(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:10514:Pumpkin methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:10517:Pumpkin methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:10519:Pumpkin methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static void linkTimeNonInherited() {
	TheGreatPumpkin = null;
/*
udanax-top.st:10531:Pumpkin class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	TheGreatPumpkin _ NULL!
*/
}
/**
 * Just return the soleInstance.
 */
public static Abraham make() {
	if (TheGreatPumpkin == null) {
		TheGreatPumpkin = new Pumpkin(1);
		TheGreatPumpkin.flockInfo((FlockInfo.remembered(TheGreatPumpkin, 0, 0)));
	}
	return TheGreatPumpkin;
/*
udanax-top.st:10536:Pumpkin class methodsFor: 'pcreate'!
{Abraham wimpy} make
	"Just return the soleInstance."
	
	TheGreatPumpkin == NULL ifTrue:
		[TheGreatPumpkin _ self create: 1.
		TheGreatPumpkin flockInfo: (FlockInfo remembered: TheGreatPumpkin with: Int32Zero with: Int32Zero)].
	^TheGreatPumpkin!
*/
}
public Pumpkin() {
/*

Generated during transformation
*/
}
}
