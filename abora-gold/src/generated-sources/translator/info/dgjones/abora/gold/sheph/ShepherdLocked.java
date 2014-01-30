/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.sheph;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.sheph.ShepherdLocked;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.stacker.StackExaminer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class ShepherdLocked extends Abraham {

/*
udanax-top.st:11116:
Abraham subclass: #ShepherdLocked
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-sheph'!
*/
/*
udanax-top.st:11120:
(ShepherdLocked getOrMakeCxxClassDescription)
	friends:
'/- friends for class ShepherdLocked -/';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:11151:
ShepherdLocked class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:11154:
(ShepherdLocked getOrMakeCxxClassDescription)
	friends:
'/- friends for class ShepherdLocked -/';
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ShepherdLocked.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public ShepherdLocked() {
	super();
/*
udanax-top.st:11127:ShepherdLocked methodsFor: 'instance creation'!
create
	super create!
*/
}
public boolean isReallyUnlocked() {
	return (StackExaminer.pointersOnStack().fetch(asOop())) == null;
/*
udanax-top.st:11133:ShepherdLocked methodsFor: 'accessing'!
{BooleanVar} isReallyUnlocked
	[^ (StackExaminer pointersOnStack fetch: self asOop) == NULL] smalltalkOnly.
	'return StackExaminer::pointersOnStack()->fetch((Int32)(void*)this) == NULL;' translateOnly.!
*/
}
/**
 * self unlock
 */
public void publicUnlock() {
/*
udanax-top.st:11139:ShepherdLocked methodsFor: 'testing locks'!
{void} publicUnlock
	"self unlock"!
*/
}
public ShepherdLocked(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:11144:ShepherdLocked methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:11147:ShepherdLocked methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static ShepherdLocked makeLocked() {
	return new ShepherdLocked();
/*
udanax-top.st:11161:ShepherdLocked class methodsFor: 'instance creation'!
{ShepherdLocked} makeLocked
	^ShepherdLocked create!
*/
}
public static ShepherdLocked makeUnlocked() {
	ShepherdLocked aLockedShepherd;
	aLockedShepherd = new ShepherdLocked();
	aLockedShepherd.publicUnlock();
	return aLockedShepherd;
/*
udanax-top.st:11164:ShepherdLocked class methodsFor: 'instance creation'!
{ShepherdLocked} makeUnlocked
	| aLockedShepherd {ShepherdLocked} |
	aLockedShepherd _ ShepherdLocked create.
	aLockedShepherd publicUnlock.
	^aLockedShepherd!
*/
}
}
