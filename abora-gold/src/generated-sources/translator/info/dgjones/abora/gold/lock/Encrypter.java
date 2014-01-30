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
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.EncrypterConstructor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.lock.EncrypterMaker;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * An Encrypter is an instantiation of some public-key encryption algorithm, along with
 * optional public and private keys. Each subclass implements a particular algorithm, such as
 * Rivest-Shamir-Adelman, in response to the encryption, decryption, and key generation
 * protocol.
 * ** obsolete documentation **
 * The algorithm is identified by a Sequence naming it. Each concrete subclass must register
 * itself during initialization time. This is handled by two macros, DECLARE_ENCRYPTER and
 * DEFINE_ENCRYPTER. DECLARE_ENCRYPTER(AClassName) defines a function that can be used to
 * create an instance. DEFINE_ENCRYPTER("identifier",AClassName) creates an EncrypterMaker
 * parametrized with that "constructor" function pointer, and stores it in the system-wide
 * table of EncrypterMakers. DECLARE_ENCRYPTER should be invoked in function scope (i.e.
 * inside a linkTimeNonInherited class method) and DEFINE_ENCRYPTER should be invoked inside
 * an Initializer (i.e. inside an initTimeNonInherited class method).
 * The pseudo-constructor to make an Encrypter takes the PackOBits identifying the algorithm,
 * and looks for a corresponding EncrypterMaker in the table. It then asks that
 * EncrypterMaker to create an instance, with the given public and private keys.
 * Encrypters are mutable objects. This allows you to create an Encrypter, generate new
 * random keys for it, make a copy, remove its private key, and pass that out for public use.
 */
public class Encrypter extends Heaper {

	protected UInt8Array myPublicKey;
	protected UInt8Array myPrivateKey;
	protected static MuTable AllEncrypterMakers;
/*
udanax-top.st:18637:
Heaper subclass: #Encrypter
	instanceVariableNames: '
		myPublicKey {UInt8Array | NULL}
		myPrivateKey {UInt8Array | NULL}'
	classVariableNames: 'AllEncrypterMakers {MuTable of: Sequence and: EncrypterMaker} '
	poolDictionaries: ''
	category: 'Xanadu-lock'!
*/
/*
udanax-top.st:18643:
Encrypter comment:
'An Encrypter is an instantiation of some public-key encryption algorithm, along with optional public and private keys. Each subclass implements a particular algorithm, such as Rivest-Shamir-Adelman, in response to the encryption, decryption, and key generation protocol. 
** obsolete documentation **
The algorithm is identified by a Sequence naming it. Each concrete subclass must register itself during initialization time. This is handled by two macros, DECLARE_ENCRYPTER and DEFINE_ENCRYPTER. DECLARE_ENCRYPTER(AClassName) defines a function that can be used to create an instance. DEFINE_ENCRYPTER("identifier",AClassName) creates an EncrypterMaker parametrized with that "constructor" function pointer, and stores it in the system-wide table of EncrypterMakers. DECLARE_ENCRYPTER should be invoked in function scope (i.e. inside a linkTimeNonInherited class method) and DEFINE_ENCRYPTER should be invoked inside an Initializer (i.e. inside an initTimeNonInherited class method).
The pseudo-constructor to make an Encrypter takes the PackOBits identifying the algorithm, and looks for a corresponding EncrypterMaker in the table. It then asks that EncrypterMaker to create an instance, with the given public and private keys.
Encrypters are mutable objects. This allows you to create an Encrypter, generate new random keys for it, make a copy, remove its private key, and pass that out for public use.'!
*/
/*
udanax-top.st:18653:
(Encrypter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:18712:
Encrypter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:18715:
(Encrypter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Encrypter.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public Encrypter(UInt8Array publicKey, UInt8Array privateKey) {
	super();
	myPublicKey = publicKey;
	myPrivateKey = privateKey;
/*
udanax-top.st:18658:Encrypter methodsFor: 'create'!
create: publicKey {UInt8Array | NULL} with: privateKey {UInt8Array | NULL}
	super create.
	myPublicKey := publicKey.
	myPrivateKey := privateKey.!
*/
}
/**
 * Decrypt data with the current private key.
 */
public UInt8Array decrypt(UInt8Array encrypted) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18666:Encrypter methodsFor: 'encrypting/decrypting'!
{UInt8Array} decrypt: encrypted {UInt8Array}
	"Decrypt data with the current private key."
	
	self subclassResponsibility!
*/
}
/**
 * Encrypt the given data with the current public key.
 */
public UInt8Array encrypt(UInt8Array clear) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18671:Encrypter methodsFor: 'encrypting/decrypting'!
{UInt8Array} encrypt: clear {UInt8Array}
	"Encrypt the given data with the current public key."
	
	self subclassResponsibility!
*/
}
public UInt8Array privateKey() {
	if (myPrivateKey == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_PRIVATE_KEY);
	}
	return myPrivateKey;
/*
udanax-top.st:18678:Encrypter methodsFor: 'keys'!
{UInt8Array} privateKey
	myPrivateKey == NULL
		ifTrue: [Heaper BLAST: #NoPrivateKey].
	^myPrivateKey!
*/
}
public UInt8Array publicKey() {
	if (myPublicKey == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_PUBLIC_KEY);
	}
	return myPublicKey;
/*
udanax-top.st:18684:Encrypter methodsFor: 'keys'!
{UInt8Array} publicKey
	myPublicKey == NULL
		ifTrue: [Heaper BLAST: #NoPublicKey].
	^myPublicKey!
*/
}
/**
 * Generate a new pair of public and private keys using the given data as a random seed.
 */
public void randomizeKeys(UInt8Array seed) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:18690:Encrypter methodsFor: 'keys'!
{void} randomizeKeys: seed {UInt8Array}
	"Generate a new pair of public and private keys using the given data as a random seed."
	
	self subclassResponsibility!
*/
}
/**
 * Change the private key.
 */
public void setPrivateKey(UInt8Array newKey) {
	myPrivateKey = newKey;
/*
udanax-top.st:18695:Encrypter methodsFor: 'keys'!
{void} setPrivateKey: newKey {UInt8Array | NULL}
	"Change the private key."
	
	myPrivateKey := newKey.!
*/
}
/**
 * Change the public key.
 */
public void setPublicKey(UInt8Array newKey) {
	myPublicKey = newKey;
/*
udanax-top.st:18700:Encrypter methodsFor: 'keys'!
{void} setPublicKey: newKey {UInt8Array | NULL}
	"Change the public key."
	
	myPublicKey := newKey.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:18707:Encrypter methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:18709:Encrypter methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * Make an encrypter of the given type with the given public and private keys. Gets the
 * requested EncrypterMaker out of the table and then asks it to make an encrypter with the
 * given key. Fails with
 * BLAST(NoSuchEncrypter) if it is not found.
 */
public static Encrypter make(Sequence identifier, UInt8Array publicKey, UInt8Array privateKey) {
	try {
		return ((EncrypterMaker) (AllEncrypterMakers.get(identifier))).makeEncrypter(publicKey, privateKey);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.NOT_IN_TABLE.equals(ex.getMessage())) {
			throw new AboraRuntimeException(AboraRuntimeException.NO_SUCH_ENCRYPTER);
		}
		else {
			throw ex;
		}
	}
/*
udanax-top.st:18720:Encrypter class methodsFor: 'pseudo constructors'!
make: identifier {Sequence}
	with: publicKey {UInt8Array default: NULL}
	with: privateKey {UInt8Array default: NULL}
	"Make an encrypter of the given type with the given public and private keys. Gets the requested EncrypterMaker out of the table and then asks it to make an encrypter with the given key. Fails with
		BLAST(NoSuchEncrypter) if it is not found."
	ScruTable problems.NotInTable
		handle: [ :boom | Heaper BLAST: #NoSuchEncrypter]
		do: [^((AllEncrypterMakers get: identifier) cast: EncrypterMaker)
			makeEncrypter: publicKey with: privateKey]!
*/
}
/**
 * Only applies in C++
 */
public static void DECLAREUENCRYPTER(String className) {
/*
udanax-top.st:18733:Encrypter class methodsFor: 'smalltalk: macros'!
DECLARE.U.ENCRYPTER: className {Symbol}
	"Only applies in C++"!
*/
}
public static void DEFINEUENCRYPTER(String identifier, String className) {
	remember((Sequence.string(identifier)), EncrypterConstructor.make((AboraSupport.findCategory(className))));
/*
udanax-top.st:18737:Encrypter class methodsFor: 'smalltalk: macros'!
DEFINE.U.ENCRYPTER: identifier {String} with: className {Symbol}
	self REQUIRES: Encrypter.
	self remember: (Sequence string: identifier)
		with: (Smalltalk at: className)!
*/
}
/**
 * In Smalltalk, the Encrypter class is used in place of the function pointer.
 */
public static Encrypter invokeFunction(Sequence publicKey, Sequence privateKey) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:18743:Encrypter class methodsFor: 'smalltalk: macros'!
{Encrypter} invokeFunction: publicKey {Sequence| NULL} with: privateKey {Sequence | NULL}
	"In Smalltalk, the Encrypter class is used in place of the function pointer."
	
	^self create: publicKey with: privateKey!
*/
}
public static void remember(Sequence identifier, EncrypterConstructor constructor) {
	EncrypterMaker maker;
	maker = new EncrypterMaker(constructor);
	AllEncrypterMakers.introduce(identifier, maker);
/*
udanax-top.st:18750:Encrypter class methodsFor: 'was protected'!
{void} remember: identifier {Sequence}
	with: constructor {EncrypterConstructor var}
	| maker {EncrypterMaker} |
	maker := EncrypterMaker create: constructor.
	AllEncrypterMakers at: identifier introduce: maker.!
*/
}
public static Encrypter make(Sequence identifier) {
	return make(identifier, null, null);
/*
udanax-top.st:18759:Encrypter class methodsFor: 'smalltalk: defaults'!
make: identifier {Sequence}
	^self make: identifier with: NULL with: NULL!
*/
}
/*
udanax-top.st:18763:Encrypter class methodsFor: 'smalltalk: defaults'!
make: identifier {Sequence} with: publicKey {Sequence}
	^self make: identifier with: publicKey with: NULL!
*/
public static void initTimeNonInherited() {
	AllEncrypterMakers = MuTable.make(SequenceSpace.make());
/*
udanax-top.st:18769:Encrypter class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: SequenceSpace.
	self REQUIRES: MuTable.
	AllEncrypterMakers := MuTable make: SequenceSpace make.!
*/
}
public static void linkTimeNonInherited() {
	AllEncrypterMakers = null;
/*
udanax-top.st:18775:Encrypter class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	AllEncrypterMakers := NULL.!
*/
}
public Encrypter() {
/*

Generated during transformation
*/
}
public Encrypter(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public static Encrypter make(Sequence identifier, UInt8Array publicKey) {
	return make(identifier, publicKey, null );
/*

Generated during transformation: AddDefaultParameter
*/
}
}
