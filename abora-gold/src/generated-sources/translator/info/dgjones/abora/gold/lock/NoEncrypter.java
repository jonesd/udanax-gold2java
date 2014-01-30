/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.lock;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.lock.NoEncrypter;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Does no encryption at all.
 */
public class NoEncrypter extends Encrypter {

/*
udanax-top.st:18779:
Encrypter subclass: #NoEncrypter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-lock'!
*/
/*
udanax-top.st:18783:
NoEncrypter comment:
'Does no encryption at all.'!
*/
/*
udanax-top.st:18785:
(NoEncrypter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:18812:
NoEncrypter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:18815:
(NoEncrypter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(NoEncrypter.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public NoEncrypter(UInt8Array publicKey, UInt8Array privateKey) {
	super(publicKey, privateKey);
/*
udanax-top.st:18790:NoEncrypter methodsFor: 'create'!
create: publicKey {UInt8Array | NULL} with: privateKey {UInt8Array | NULL}
	super create: publicKey with: privateKey.!
*/
}
public UInt8Array decrypt(UInt8Array encrypted) {
	return encrypted;
/*
udanax-top.st:18796:NoEncrypter methodsFor: 'encrypting/decrypting'!
{UInt8Array} decrypt: encrypted {UInt8Array}
	^encrypted!
*/
}
public UInt8Array encrypt(UInt8Array clear) {
	return (UInt8Array) clear.copy();
/*
udanax-top.st:18800:NoEncrypter methodsFor: 'encrypting/decrypting'!
{UInt8Array} encrypt: clear {UInt8Array}
	^clear copy cast: UInt8Array!
*/
}
public void randomizeKeys(UInt8Array seed) {
	setPublicKey((UInt8Array.string("public")));
	setPrivateKey((UInt8Array.string("private")));
/*
udanax-top.st:18806:NoEncrypter methodsFor: 'keys'!
{void} randomizeKeys: seed {UInt8Array unused}
	self setPublicKey: (UInt8Array string: 'public').
	self setPrivateKey: (UInt8Array string: 'private').!
*/
}
public static void initTimeNonInherited() {
	DEFINEUENCRYPTER("NoEncrypter", NO_ENCRYPTER);
/*
udanax-top.st:18820:NoEncrypter class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self DEFINE.U.ENCRYPTER: 'NoEncrypter' with: #NoEncrypter!
*/
}
public static void linkTimeNonInherited() {
	DECLAREUENCRYPTER(NO_ENCRYPTER);
/*
udanax-top.st:18824:NoEncrypter class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	self DECLARE.U.ENCRYPTER: #NoEncrypter!
*/
}
public static Encrypter make(UInt8Array publicKey, UInt8Array privateKey) {
	return new NoEncrypter(publicKey, privateKey);
/*
udanax-top.st:18830:NoEncrypter class methodsFor: 'create'!
{Encrypter} make: publicKey {UInt8Array | NULL} with: privateKey {UInt8Array | NULL}
	^ self create: publicKey with: privateKey.!
*/
}
public NoEncrypter() {
/*

Generated during transformation
*/
}
public NoEncrypter(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
