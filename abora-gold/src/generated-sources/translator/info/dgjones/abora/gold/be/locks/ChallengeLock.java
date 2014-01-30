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
import info.dgjones.abora.gold.be.locks.ChallengeLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.nadmin.FeChallengeLockSmith;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * A ChallengeLock challenges the client with a random piece of data that has been encrypted
 * with a publicKey, using an algorithm identified by the encrypterName. The client must
 * decrypt it using the corresponding private key and respond with the decrypted challenge.
 * If it matches the original random data, then the lock will open. The encrypterName and the
 * publicKey are stored in the club`s ChallengeLockSmith.
 */
public class ChallengeLock extends Lock {

	protected UInt8Array myChallenge;
	protected UInt8Array myResponse;
/*
udanax-top.st:28019:
Lock subclass: #ChallengeLock
	instanceVariableNames: '
		myChallenge {UInt8Array}
		myResponse {UInt8Array}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Locks'!
*/
/*
udanax-top.st:28025:
ChallengeLock comment:
'A ChallengeLock challenges the client with a random piece of data that has been encrypted with a publicKey, using an algorithm identified by the encrypterName. The client must decrypt it using the corresponding private key and respond with the decrypted challenge. If it matches the original random data, then the lock will open. The encrypterName and the publicKey are stored in the club`s ChallengeLockSmith. '!
*/
/*
udanax-top.st:28027:
(ChallengeLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:28057:
ChallengeLock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28060:
(ChallengeLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ChallengeLock.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public ChallengeLock(ID allegedID, FeLockSmith lockSmith, UInt8Array challenge, UInt8Array response) {
	super(allegedID, lockSmith);
	myChallenge = challenge;
	myResponse = response;
/*
udanax-top.st:28032:ChallengeLock methodsFor: 'private: create'!
create: allegedID {ID}
	with: lockSmith {FeLockSmith}
	with: challenge {UInt8Array}
	with: response {UInt8Array}
	super create: allegedID with: lockSmith.
	myChallenge := challenge.
	myResponse := response.!
*/
}
/**
 * Essential.  The challenge which must be signed correctly to open the lock.
 */
public UInt8Array challenge() {
	return (UInt8Array) myChallenge.copy();
/*
udanax-top.st:28043:ChallengeLock methodsFor: 'accessing'!
{UInt8Array CLIENT login} challenge
	"Essential.  The challenge which must be signed correctly to open the lock."
	^myChallenge copy cast: UInt8Array!
*/
}
/**
 * Essential.  The correctly signed challenge will open the lock.
 */
public FeKeyMaster response(PrimIntArray signedChallenge) {
	if ( ! (fetchLoginClubID() != null && (myResponse.contentsEqual(((UInt8Array) signedChallenge))))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_CORRECTLY_SIGNED);
	}
	return makeKeyMaster();
/*
udanax-top.st:28048:ChallengeLock methodsFor: 'accessing'!
{FeKeyMaster CLIENT login} response: signedChallenge {PrimIntArray}
	"Essential.  The correctly signed challenge will open the lock."
	
	(self fetchLoginClubID ~~ NULL
			and: [myResponse contentsEqual: (signedChallenge cast: UInt8Array)]) ifFalse:
		[Heaper BLAST: #NotCorrectlySigned].
	^self makeKeyMaster!
*/
}
public static ChallengeLock make(ID loginID, FeChallengeLockSmith lockSmith, UInt8Array response) {
	return new ChallengeLock(loginID, lockSmith, ((Encrypter.make((Sequence.numbers(lockSmith.encrypterName())), lockSmith.publicKey())).encrypt(response)), ((UInt8Array) response.copy()));
/*
udanax-top.st:28065:ChallengeLock class methodsFor: 'pseudo constructors'!
make: loginID {ID | NULL}
	with: lockSmith {FeChallengeLockSmith}
	with: response {UInt8Array}
	
	^self create: loginID
		with: lockSmith
		with: ((Encrypter 
				make: (Sequence numbers: lockSmith encrypterName)
				with: lockSmith publicKey)
			encrypt: response)
		with: (response copy cast: UInt8Array)!
*/
}
/**
 * {UInt8Array CLIENT} challenge
 * {FeKeyMaster CLIENT} response: signedChallenge {UInt8Array}
 */
public static void infostProtocol() {
/*
udanax-top.st:28079:ChallengeLock class methodsFor: 'smalltalk: system'!
info.stProtocol
"{UInt8Array CLIENT} challenge
{FeKeyMaster CLIENT} response: signedChallenge {UInt8Array}
"!
*/
}
public ChallengeLock() {
/*

Generated during transformation
*/
}
public ChallengeLock(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
