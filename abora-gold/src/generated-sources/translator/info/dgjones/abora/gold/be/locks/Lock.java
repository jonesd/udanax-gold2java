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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * To login to a club, you ask the server for a Lock. If you send the right message to the
 * Lock, it will return you a new KeyMaster with the authority of the club. Each subclass of
 * Lock defines its own protocol for opening.
 * For each kind of Lock, there is a corresponding kind of LockSmith which creates it. Each
 * ClubManager has a LockSmith sub-document, and when you ask the server for a Lock to that
 * club, it asks the club`s LockSmith document Wrapper to create a newLock. The LockSmith
 * then creates the corresponding kind of Lock. It may also use information stored in the
 * LockSmith document, such as a password or scramblerName.
 */
public class Lock extends Heaper {

	protected ID myLoginClubID;
	protected FeLockSmith myLockSmith;
/*
udanax-top.st:27921:
Heaper subclass: #Lock
	instanceVariableNames: '
		myLoginClubID {ID}
		myLockSmith {FeLockSmith}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Locks'!
*/
/*
udanax-top.st:27927:
Lock comment:
'To login to a club, you ask the server for a Lock. If you send the right message to the Lock, it will return you a new KeyMaster with the authority of the club. Each subclass of Lock defines its own protocol for opening. 
For each kind of Lock, there is a corresponding kind of LockSmith which creates it. Each ClubManager has a LockSmith sub-document, and when you ask the server for a Lock to that club, it asks the club`s LockSmith document Wrapper to create a newLock. The LockSmith then creates the corresponding kind of Lock. It may also use information stored in the LockSmith document, such as a password or scramblerName.'!
*/
/*
udanax-top.st:27931:
(Lock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Lock.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Lock(ID loginID, FeLockSmith lockSmith) {
	super();
	myLoginClubID = loginID;
	myLockSmith = lockSmith;
/*
udanax-top.st:27936:Lock methodsFor: 'create'!
create: loginID {ID} with: lockSmith {FeLockSmith}
	super create.
	myLoginClubID := loginID.
	myLockSmith := lockSmith.!
*/
}
/**
 * The lock is opened - make the right KeyMaster
 */
public FeKeyMaster makeKeyMaster() {
	Someone.hack();
	/* This should eventually be done by manipulating the cookbooks */
	if ( ! (FeSession.current().isLoggedIn())) {
		FeSession.current().setInitialLogin(myLoginClubID);
	}
	return FeKeyMaster.make(myLoginClubID);
/*
udanax-top.st:27944:Lock methodsFor: 'server accessing'!
{FeKeyMaster} makeKeyMaster
	"The lock is opened - make the right KeyMaster"
	
	self hack. "This should eventually be done by manipulating the cookbooks"
	FeSession current isLoggedIn ifFalse:
		[FeSession current setInitialLogin: myLoginClubID].
	^FeKeyMaster make: myLoginClubID!
*/
}
/**
 * The ID of the club whose authority you can get by opening this lock.
 */
public ID fetchLoginClubID() {
	return myLoginClubID;
/*
udanax-top.st:27954:Lock methodsFor: 'protected:'!
{ID} fetchLoginClubID
	"The ID of the club whose authority you can get by opening this lock."
	
	^myLoginClubID!
*/
}
/**
 * Essential. The LockSmith which made this Lock.
 */
public FeLockSmith lockSmith() {
	return myLockSmith;
/*
udanax-top.st:27959:Lock methodsFor: 'protected:'!
{FeLockSmith} lockSmith
	"Essential. The LockSmith which made this Lock."
	
	^myLockSmith!
*/
}
/**
 * @deprecated
 */
public ID loginClubID() {
	throw new PasseException();
/*
udanax-top.st:27966:Lock methodsFor: 'smalltalk: passe'!
{ID} loginClubID
	self passe "fetch"!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:27972:Lock methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:27974:Lock methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public Lock() {
/*

Generated during transformation
*/
}
public Lock(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
