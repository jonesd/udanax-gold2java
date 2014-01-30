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
import info.dgjones.abora.gold.be.locks.MatchLock;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeMatchLockSmith;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * The correct password will open the lock. The password is actually stored in the club`s
 * MatchLockSmith in scrambled form, using a Scrambler identified by scramblerName(). The
 * scrambled cleartext supplied as a password is compared to the scrambledPassword in the
 * MatchLockSmith. If they match, the lock is opened.
 * The actual process is a bit more complicated than this. The user supplies a password in
 * clear, which is encrypted with the current system public key and then sent to the server.
 * There, it is first decrypted with the private key known only to the server. It is then
 * scrambled and compared with the scrambled password stored in the MatchLockSmith of the
 * club. This procedure both avoids sending passwords in clear over the network, and also
 * allows the MatchLockSmith to be made readable without compromising security.
 */
public class MatchLock extends Lock {

/*
udanax-top.st:28084:
Lock subclass: #MatchLock
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Locks'!
*/
/*
udanax-top.st:28088:
MatchLock comment:
'The correct password will open the lock. The password is actually stored in the club`s MatchLockSmith in scrambled form, using a Scrambler identified by scramblerName(). The scrambled cleartext supplied as a password is compared to the scrambledPassword in the MatchLockSmith. If they match, the lock is opened. 
The actual process is a bit more complicated than this. The user supplies a password in clear, which is encrypted with the current system public key and then sent to the server. There, it is first decrypted with the private key known only to the server. It is then scrambled and compared with the scrambled password stored in the MatchLockSmith of the club. This procedure both avoids sending passwords in clear over the network, and also allows the MatchLockSmith to be made readable without compromising security.'!
*/
/*
udanax-top.st:28092:
(MatchLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:28115:
MatchLock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:28118:
(MatchLock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MatchLock.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Send the encrypted password to the server to be checked.
 * NOTE: (for protocol review) The password must have been encrypted using a
 * (yet-to-be-defined) front end library function, since this sort of front end computation
 * can't be done with Promises.
 */
public FeKeyMaster encryptedPassword(PrimIntArray encrypted) {
	FeServer cs;
	cs = ((FeServer) CurrentServer.fluidGet());
	if ( ! (fetchLoginClubID() != null && (((FeMatchLockSmith) lockSmith()).scrambledPassword().contentsEqual((cs.encrypter().decrypt(((UInt8Array) encrypted))))))) {
		throw new AboraRuntimeException(AboraRuntimeException.DOES_NOT_MATCH);
	}
	return makeKeyMaster();
/*
udanax-top.st:28097:MatchLock methodsFor: 'accessing'!
{FeKeyMaster CLIENT login} encryptedPassword: encrypted {PrimIntArray}
	"Send the encrypted password to the server to be checked.
	NOTE: (for protocol review) The password must have been encrypted using a (yet-to-be-defined) front end library function, since this sort of front end computation can't be done with Promises."
	| cs {FeServer} |
	cs := CurrentServer fluidGet.
	(self fetchLoginClubID ~~ NULL
			and: [(self lockSmith cast: FeMatchLockSmith) scrambledPassword
				contentsEqual: (cs encrypter decrypt: (encrypted cast: UInt8Array))])
		ifFalse: [Heaper BLAST: #DoesNotMatch].
	^self makeKeyMaster!
*/
}
public MatchLock(ID loginID, FeMatchLockSmith lockSmith) {
	super(loginID, lockSmith);
/*
udanax-top.st:28110:MatchLock methodsFor: 'private: create'!
create: loginID {ID} with: lockSmith {FeMatchLockSmith}
	super create: loginID with: lockSmith!
*/
}
/*
udanax-top.st:28123:MatchLock class methodsFor: 'exceptions: exceptions'!
problems.PasswordDoesNotMatch
	^self signals: #(PasswordDoesNotMatch)!
*/
public static MatchLock make(ID clubID, FeMatchLockSmith lockSmith) {
	return new MatchLock(clubID, lockSmith);
/*
udanax-top.st:28130:MatchLock class methodsFor: 'pseudo constructors'!
make: clubID {ID | NULL} with: lockSmith {FeMatchLockSmith}
	^self create: clubID with: lockSmith!
*/
}
/**
 * {FeKeyMaster CLIENT} encryptedPassword: encrypted {UInt8Array}
 */
public static void infostProtocol() {
/*
udanax-top.st:28136:MatchLock class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeKeyMaster CLIENT} encryptedPassword: encrypted {UInt8Array}
"!
*/
}
public MatchLock() {
/*

Generated during transformation
*/
}
public MatchLock(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
