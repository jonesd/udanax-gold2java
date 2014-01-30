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
import info.dgjones.abora.gold.be.locks.MultiLock;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeMultiLockSmith;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A MultiLock allows the client to open the lock with any of a list of Locks. This allows a
 * Club to have different passwords for different people; or, the Locks can use different
 * kinds of native authentication systems such as NIS or Kerberos.
 */
public class MultiLock extends Lock {

	protected ImmuTable myLocks;
/*
udanax-top.st:28140:
Lock subclass: #MultiLock
	instanceVariableNames: 'myLocks {ImmuTable of: Sequence and: Lock}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Locks'!
*/
/*
udanax-top.st:28144:
MultiLock comment:
'A MultiLock allows the client to open the lock with any of a list of Locks. This allows a Club to have different passwords for different people; or, the Locks can use different kinds of native authentication systems such as NIS or Kerberos.'!
*/
/*
udanax-top.st:28146:
(MultiLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:28169:
MultiLock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28172:
(MultiLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MultiLock.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public MultiLock(ID loginID, FeMultiLockSmith lockSmith, ImmuTable locks) {
	super(loginID, lockSmith);
	myLocks = locks;
/*
udanax-top.st:28151:MultiLock methodsFor: 'create'!
create: loginID {ID} with: lockSmith {FeMultiLockSmith} with: locks {ImmuTable of: Lock}
	super create: loginID with: lockSmith.
	myLocks := locks!
*/
}
/**
 * Get the named lock. You don't get any authority through a MultiLock directly, you merely
 * get a Lock from which you can get authority.
 */
public Lock lock(Sequence name) {
	return (Lock) (myLocks.get(name));
/*
udanax-top.st:28158:MultiLock methodsFor: 'accessing'!
{Lock CLIENT login} lock: name {Sequence}
	"Get the named lock. You don't get any authority through a MultiLock directly, you merely get a Lock from which you can get authority."
	
	^(myLocks get: name) cast: Lock!
*/
}
/**
 * Essential. The names identifying the locks in the list
 */
public SequenceRegion lockNames() {
	return ((FeMultiLockSmith) lockSmith()).lockSmithNames();
/*
udanax-top.st:28163:MultiLock methodsFor: 'accessing'!
{SequenceRegion CLIENT login} lockNames
	"Essential. The names identifying the locks in the list"
	
	^(self lockSmith cast: FeMultiLockSmith) lockSmithNames!
*/
}
public static MultiLock make(ID loginID, FeMultiLockSmith lockSmith, ImmuTable locks) {
	return new MultiLock(loginID, lockSmith, locks);
/*
udanax-top.st:28177:MultiLock class methodsFor: 'create'!
make: loginID {ID | NULL} with: lockSmith {FeMultiLockSmith} with: locks {ImmuTable of: Lock}
	^self create: loginID with: lockSmith with: locks!
*/
}
/**
 * {Lock CLIENT} lock: name {Sequence}
 * {SequenceSpace CLIENT} lockNames
 */
public static void infostProtocol() {
/*
udanax-top.st:28183:MultiLock class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Lock CLIENT} lock: name {Sequence}
{SequenceSpace CLIENT} lockNames
"!
*/
}
public MultiLock() {
/*

Generated during transformation
*/
}
public MultiLock(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
