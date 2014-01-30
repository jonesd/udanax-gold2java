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
import info.dgjones.abora.gold.java.missing.EncrypterConstructor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.lock.EncrypterMaker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Contains a pointer to a function used to create an instance of a particular kind of
 * Encrypter.
 * Each concrete Encrypter subclass should create a corresponding EncrypterMaker object and
 * register it in a table, with the name of the encryption algorithm. This should be done
 * using the DECLARE_ENCRYPTER and DEFINE_ENCRYPTER macros.
 */
public class EncrypterMaker extends Heaper {

	protected EncrypterConstructor myConstructor;
/*
udanax-top.st:18834:
Heaper subclass: #EncrypterMaker
	instanceVariableNames: 'myConstructor {EncrypterConstructor var}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-lock'!
*/
/*
udanax-top.st:18838:
EncrypterMaker comment:
'Contains a pointer to a function used to create an instance of a particular kind of Encrypter. 
Each concrete Encrypter subclass should create a corresponding EncrypterMaker object and register it in a table, with the name of the encryption algorithm. This should be done using the DECLARE_ENCRYPTER and DEFINE_ENCRYPTER macros.'!
*/
/*
udanax-top.st:18842:
(EncrypterMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EncrypterMaker.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public EncrypterMaker(EncrypterConstructor constructor) {
	super();
	myConstructor = constructor;
/*
udanax-top.st:18847:EncrypterMaker methodsFor: 'create'!
create: constructor {EncrypterConstructor var}
	super create.
	myConstructor := constructor.!
*/
}
/**
 * Make an instance of this kind of encrypter, with the given public and private keys.
 */
public Encrypter makeEncrypter(UInt8Array publicKey, UInt8Array privateKey) {
	return myConstructor.invokeFunction(publicKey, privateKey);
/*
udanax-top.st:18854:EncrypterMaker methodsFor: 'accessing'!
{Encrypter} makeEncrypter: publicKey {UInt8Array | NULL} with: privateKey {UInt8Array | NULL}
	"Make an instance of this kind of encrypter, with the given public and private keys."
	
	^myConstructor invokeFunction: publicKey with: privateKey!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:18861:EncrypterMaker methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:18863:EncrypterMaker methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public EncrypterMaker() {
/*

Generated during transformation
*/
}
public EncrypterMaker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
