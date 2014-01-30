/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nadmin;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.ChallengeLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeChallengeLockSmith;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Makes ChallengeLocks; see the comment there
 */
public class FeChallengeLockSmith extends FeLockSmith {

	protected static FeWrapperSpec TheChallengeLockSmithSpec;
/*
udanax-top.st:24650:
FeLockSmith subclass: #FeChallengeLockSmith
	instanceVariableNames: ''
	classVariableNames: 'TheChallengeLockSmithSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:24654:
FeChallengeLockSmith comment:
'Makes ChallengeLocks; see the comment there'!
*/
/*
udanax-top.st:24656:
(FeChallengeLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24689:
FeChallengeLockSmith class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24692:
(FeChallengeLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeChallengeLockSmith.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The type of encrypter used to create encrypted challenges.
 */
public UInt8Array encrypterName() {
	return (UInt8Array) ((FeArrayBundle) (((FeEdition) (edition().get((Sequence.string("ChallengeLockSmith:EncrypterName"))))).retrieve().theOne())).array();
/*
udanax-top.st:24661:FeChallengeLockSmith methodsFor: 'accessing'!
{UInt8Array CLIENT} encrypterName
	"The type of encrypter used to create encrypted challenges."
	^((((self edition get: (Sequence string: 'ChallengeLockSmith:EncrypterName'))
		cast: FeEdition) retrieve theOne) cast: FeArrayBundle) array cast: UInt8Array!
*/
}
/**
 * The public key used to construct challenges.
 */
public UInt8Array publicKey() {
	return (UInt8Array) ((FeArrayBundle) (((FeEdition) (edition().get((Sequence.string("ChallengeLockSmith:PublicKey"))))).retrieve().theOne())).array();
/*
udanax-top.st:24667:FeChallengeLockSmith methodsFor: 'accessing'!
{UInt8Array CLIENT} publicKey
	"The public key used to construct challenges."
	^((((self edition get: (Sequence string: 'ChallengeLockSmith:PublicKey'))
		cast: FeEdition) retrieve theOne) cast: FeArrayBundle) array cast: UInt8Array!
*/
}
public Lock newLock(ID clubID) {
	Someone.thingToDo();
	/* Make this random */
	return ChallengeLock.make(clubID, this, (UInt8Array.string("random")));
/*
udanax-top.st:24675:FeChallengeLockSmith methodsFor: 'server locks'!
{Lock} newLock: clubID {ID | NULL}
	self thingToDo. "Make this random"
	^ChallengeLock make: clubID
		with: self
		with: (UInt8Array string: 'random')!
*/
}
public FeChallengeLockSmith(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24684:FeChallengeLockSmith methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static FeChallengeLockSmith make(PrimIntArray publicKey, Sequence encrypterName) {
	FeEdition result;
	result = FeEdition.fromOne((Sequence.string("ChallengeLockSmith:PublicKey")), (FeEdition.fromArray(((UInt8Array) publicKey))));
	result = result.with((Sequence.string("ChallengeLockSmith:EncrypterName")), (FeEdition.fromArray(encrypterName.integers())));
	return construct(result);
/*
udanax-top.st:24697:FeChallengeLockSmith class methodsFor: 'pseudo constructors'!
{FeChallengeLockSmith CLIENT} make: publicKey {PrimIntArray}
	with: encrypterName {Sequence}
	
	| result {FeEdition} |
	result := FeEdition fromOne: (Sequence string: 'ChallengeLockSmith:PublicKey')
		with: (FeEdition fromArray: (publicKey cast: UInt8Array)).
	result := result
		with: (Sequence string: 'ChallengeLockSmith:EncrypterName')
		with: (FeEdition fromArray: encrypterName integers).
	^self construct: result!
*/
}
public static FeWrapperSpec spec() {
	return TheChallengeLockSmithSpec;
/*
udanax-top.st:24708:FeChallengeLockSmith class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheChallengeLockSmithSpec!
*/
}
public static boolean check(FeEdition edition) {
	return (edition.domain().isEqual(((Sequence.string("ChallengeLockSmith:EncrypterName")).asRegion().with((Sequence.string("ChallengeLockSmith:PublicKey")))))) && ((FeWrapper.checkSubSequence(edition, (Sequence.string("ChallengeLockSmith:EncrypterName")), true)) && (FeWrapper.checkSubSequence(edition, (Sequence.string("ChallengeLockSmith:PublicKey")), true)));
/*
udanax-top.st:24714:FeChallengeLockSmith class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	
	^(edition domain isEqual: ((Sequence string: 'ChallengeLockSmith:EncrypterName') asRegion
			with: (Sequence string: 'ChallengeLockSmith:PublicKey')))
		and: [(FeWrapper checkSubSequence: edition
			with: (Sequence string: 'ChallengeLockSmith:EncrypterName')
			with: true)
		and: [FeWrapper checkSubSequence: edition
			with: (Sequence string: 'ChallengeLockSmith:PublicKey')
			with: true]]!
*/
}
public static FeChallengeLockSmith construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeChallengeLockSmith) (makeWrapper(edition));
/*
udanax-top.st:24725:FeChallengeLockSmith class methodsFor: 'private: wrapping'!
{FeChallengeLockSmith} construct: edition {FeEdition}
	
	self spec endorse: edition.
	^ (self makeWrapper: edition) cast: FeChallengeLockSmith!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeChallengeLockSmith(edition, spec());
/*
udanax-top.st:24730:FeChallengeLockSmith class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheChallengeLockSmithSpec = wrap;
/*
udanax-top.st:24734:FeChallengeLockSmith class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheChallengeLockSmithSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("ChallengeLockSmith", "LockSmith", FE_CHALLENGE_LOCK_SMITH);
/*
udanax-top.st:24740:FeChallengeLockSmith class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'ChallengeLockSmith'
		with: 'LockSmith'
		with: #FeChallengeLockSmith.!
*/
}
public static void linkTimeNonInherited() {
	TheChallengeLockSmithSpec = null;
/*
udanax-top.st:24746:FeChallengeLockSmith class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheChallengeLockSmithSpec := NULL.!
*/
}
/**
 * {PrimIntegerArray CLIENT} encrypterName
 * {UInt8Array CLIENT} publicKey
 */
public static void infostProtocol() {
/*
udanax-top.st:24752:FeChallengeLockSmith class methodsFor: 'smalltalk: system'!
info.stProtocol
"{PrimIntegerArray CLIENT} encrypterName
{UInt8Array CLIENT} publicKey
"!
*/
}
public FeChallengeLockSmith() {
/*

Generated during transformation
*/
}
public FeChallengeLockSmith(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
