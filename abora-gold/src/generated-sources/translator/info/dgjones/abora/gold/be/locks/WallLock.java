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
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.be.locks.WallLock;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A Wall cannot be opened. Sorry, dude!!!!
 * Clubs can have WallLockSmiths for a variety of reasons. Clubs that represent groups of
 * users, and to which noone should be able to login directly (only as a member using
 * loginToSuperClub), will have WallLockSmiths. Or, if you want to make a document read-only,
 * remove all the members from its editClub, make it self-reading, and put a WallLockSmith on
 * it; then, noone can login to the club, either directly or as a member, and noone can
 * change it.
 */
public class WallLock extends Lock {

/*
udanax-top.st:28188:
Lock subclass: #WallLock
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Locks'!
*/
/*
udanax-top.st:28192:
WallLock comment:
'A Wall cannot be opened. Sorry, dude!!!!
Clubs can have WallLockSmiths for a variety of reasons. Clubs that represent groups of users, and to which noone should be able to login directly (only as a member using loginToSuperClub), will have WallLockSmiths. Or, if you want to make a document read-only, remove all the members from its editClub, make it self-reading, and put a WallLockSmith on it; then, noone can login to the club, either directly or as a member, and noone can change it. '!
*/
/*
udanax-top.st:28196:
(WallLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:28206:
WallLock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28209:
(WallLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WallLock.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public WallLock(ID loginID, FeLockSmith lockSmith) {
	super(loginID, lockSmith);
/*
udanax-top.st:28201:WallLock methodsFor: 'private: create'!
create: loginID {ID} with: lockSmith {FeLockSmith}
	super create: loginID with: lockSmith!
*/
}
public static WallLock make(ID clubID, FeLockSmith lockSmith) {
	return new WallLock(clubID, lockSmith);
/*
udanax-top.st:28214:WallLock class methodsFor: 'pseudo constructors'!
make: clubID {ID | NULL} with: lockSmith {FeLockSmith}
	^self create: clubID with: lockSmith!
*/
}
public WallLock() {
/*

Generated during transformation
*/
}
public WallLock(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
