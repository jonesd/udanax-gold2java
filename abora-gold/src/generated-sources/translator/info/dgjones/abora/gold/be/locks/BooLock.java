/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.locks;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A BooLock is very easy to open. Just say "boo".
 * Since anyone can get in, only public clubs with little authority, such as System Public,
 * should have BooLockSmiths.
 */
public class BooLock extends Lock {

/*
udanax-top.st:27976:
Lock subclass: #BooLock
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Locks'!
*/
/*
udanax-top.st:27980:
BooLock comment:
'A BooLock is very easy to open. Just say "boo". 
Since anyone can get in, only public clubs with little authority, such as System Public, should have BooLockSmiths.'!
*/
/*
udanax-top.st:27984:
(BooLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:28001:
BooLock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28004:
(BooLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BooLock.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  This is a very easy lock to open. Just say `boo'.
 */
public FeKeyMaster boo() {
	return makeKeyMaster();
/*
udanax-top.st:27989:BooLock methodsFor: 'accessing'!
{FeKeyMaster CLIENT login} boo
	"Essential.  This is a very easy lock to open. Just say `boo'."
	^self makeKeyMaster!
*/
}
public BooLock(ID clubID, FeLockSmith lockSmith) {
	super(clubID, lockSmith);
/*
udanax-top.st:27996:BooLock methodsFor: 'private: create'!
create: clubID {ID} with: lockSmith {FeLockSmith}
	super create: clubID with: lockSmith!
*/
}
public static BooLock make(ID clubID, FeLockSmith lockSmith) {
	return new BooLock(clubID, lockSmith);
/*
udanax-top.st:28009:BooLock class methodsFor: 'pseudo constructors'!
make: clubID {ID} with: lockSmith {FeLockSmith}
	^self create: clubID with: lockSmith!
*/
}
/**
 * {FeKeyMaster CLIENT} boo
 */
public static void infostProtocol() {
/*
udanax-top.st:28015:BooLock class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeKeyMaster CLIENT} boo
"!
*/
}
public BooLock() {
/*

Generated during transformation
*/
}
public BooLock(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
