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
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.be.locks.MatchLock;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeMatchLockSmith;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Makes MatchLocks; see the comment there
 */
public class FeMatchLockSmith extends FeLockSmith {

	protected static FeWrapperSpec TheMatchLockSmithSpec;
/*
udanax-top.st:24757:
FeLockSmith subclass: #FeMatchLockSmith
	instanceVariableNames: ''
	classVariableNames: 'TheMatchLockSmithSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:24761:
FeMatchLockSmith comment:
'Makes MatchLocks; see the comment there'!
*/
/*
udanax-top.st:24763:
(FeMatchLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24793:
FeMatchLockSmith class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24796:
(FeMatchLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeMatchLockSmith.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The password in scrambled form. If the scrambler is any good, this should be meaningless.
 */
public UInt8Array scrambledPassword() {
	return (UInt8Array) ((FeArrayBundle) (((FeEdition) (edition().get((Sequence.string("MatchLockSmith:ScrambledPassword"))))).retrieve().theOne())).array();
/*
udanax-top.st:24768:FeMatchLockSmith methodsFor: 'accessing'!
{UInt8Array CLIENT} scrambledPassword
	"The password in scrambled form. If the scrambler is any good, this should be meaningless."
	^((((self edition get: (Sequence string: 'MatchLockSmith:ScrambledPassword'))
		cast: FeEdition) retrieve theOne) cast: FeArrayBundle) array cast: UInt8Array!
*/
}
/**
 * The name of scrambler being used to scramble the password.
 */
public UInt8Array scramblerName() {
	return (UInt8Array) ((FeArrayBundle) (((FeEdition) (edition().get((Sequence.string("MatchLockSmith:ScramblerName"))))).retrieve().theOne())).array();
/*
udanax-top.st:24774:FeMatchLockSmith methodsFor: 'accessing'!
{UInt8Array CLIENT} scramblerName
	"The name of scrambler being used to scramble the password."
	^((((self edition get: (Sequence string: 'MatchLockSmith:ScramblerName'))
		cast: FeEdition) retrieve theOne) cast: FeArrayBundle) array cast: UInt8Array!
*/
}
public Lock newLock(ID clubID) {
	return MatchLock.make(clubID, this);
/*
udanax-top.st:24782:FeMatchLockSmith methodsFor: 'server locks'!
{Lock} newLock: clubID {ID | NULL}
	^MatchLock make: clubID with: self!
*/
}
public FeMatchLockSmith(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24788:FeMatchLockSmith methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static boolean check(FeEdition edition) {
	return (edition.domain().isEqual(((Sequence.string("MatchLockSmith:ScramblerName")).asRegion().with((Sequence.string("MatchLockSmith:ScrambledPassword")))))) && ((FeWrapper.checkSubSequence(edition, (Sequence.string("MatchLockSmith:ScramblerName")), true)) && (FeWrapper.checkSubSequence(edition, (Sequence.string("MatchLockSmith:ScrambledPassword")), true)));
/*
udanax-top.st:24801:FeMatchLockSmith class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	
	^(edition domain isEqual: ((Sequence string: 'MatchLockSmith:ScramblerName') asRegion
			with: (Sequence string: 'MatchLockSmith:ScrambledPassword')))
		and: [(FeWrapper checkSubSequence: edition
			with: (Sequence string: 'MatchLockSmith:ScramblerName')
			with: true)
		and: [FeWrapper checkSubSequence: edition
			with: (Sequence string: 'MatchLockSmith:ScrambledPassword')
			with: true]]!
*/
}
public static FeMatchLockSmith construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeMatchLockSmith) (makeWrapper(edition));
/*
udanax-top.st:24812:FeMatchLockSmith class methodsFor: 'private: wrapping'!
{FeMatchLockSmith} construct: edition {FeEdition}
	
	self spec endorse: edition.
	^ (self makeWrapper: edition) cast: FeMatchLockSmith!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeMatchLockSmith(edition, spec());
/*
udanax-top.st:24817:FeMatchLockSmith class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheMatchLockSmithSpec = wrap;
/*
udanax-top.st:24821:FeMatchLockSmith class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheMatchLockSmithSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("MatchLockSmith", "LockSmith", FE_MATCH_LOCK_SMITH);
/*
udanax-top.st:24827:FeMatchLockSmith class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'MatchLockSmith'
		with: 'LockSmith'
		with: #FeMatchLockSmith.!
*/
}
public static void linkTimeNonInherited() {
	TheMatchLockSmithSpec = null;
/*
udanax-top.st:24833:FeMatchLockSmith class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheMatchLockSmithSpec := NULL.!
*/
}
public static FeMatchLockSmith make(PrimIntArray scrambledPassword, Sequence scramblerName) {
	FeEdition result;
	result = FeEdition.fromOne((Sequence.string("MatchLockSmith:ScrambledPassword")), (FeEdition.fromArray(((UInt8Array) scrambledPassword))));
	result = result.with((Sequence.string("MatchLockSmith:ScramblerName")), (FeEdition.fromArray(scramblerName.integers())));
	return construct(result);
/*
udanax-top.st:24839:FeMatchLockSmith class methodsFor: 'pseudo constructors'!
{FeMatchLockSmith CLIENT} make: scrambledPassword {PrimIntArray}
	with: scramblerName {Sequence}
	
	| result {FeEdition} |
	result := FeEdition fromOne: (Sequence string: 'MatchLockSmith:ScrambledPassword')
		with: (FeEdition fromArray: (scrambledPassword cast: UInt8Array)).
	result := result
		with: (Sequence string: 'MatchLockSmith:ScramblerName')
		with: (FeEdition fromArray: scramblerName integers).
	^self construct: result!
*/
}
public static FeWrapperSpec spec() {
	return TheMatchLockSmithSpec;
/*
udanax-top.st:24850:FeMatchLockSmith class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheMatchLockSmithSpec!
*/
}
/**
 * {UInt8Array CLIENT} scrambledPassword
 * {PrimIntegerArray CLIENT} scramblerName
 */
public static void infostProtocol() {
/*
udanax-top.st:24856:FeMatchLockSmith class methodsFor: 'smalltalk: system'!
info.stProtocol
"{UInt8Array CLIENT} scrambledPassword
{PrimIntegerArray CLIENT} scramblerName
"!
*/
}
public FeMatchLockSmith() {
/*

Generated during transformation
*/
}
public FeMatchLockSmith(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
