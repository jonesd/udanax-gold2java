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
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeBooLockSmith;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Makes BooLocks; see the comment there
 */
public class FeBooLockSmith extends FeLockSmith {

	protected static FeWrapperSpec TheBooLockSmithSpec;
/*
udanax-top.st:24574:
FeLockSmith subclass: #FeBooLockSmith
	instanceVariableNames: ''
	classVariableNames: 'TheBooLockSmithSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:24578:
FeBooLockSmith comment:
'Makes BooLocks; see the comment there'!
*/
/*
udanax-top.st:24580:
(FeBooLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24598:
FeBooLockSmith class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24601:
(FeBooLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeBooLockSmith.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Make a WallLock if clubID is NULL
 */
public Lock newLock(ID clubID) {
	if (clubID == null) {
		return FeWallLockSmith.make().newLock(null);
	}
	else {
		return BooLock.make(clubID, this);
	}
/*
udanax-top.st:24585:FeBooLockSmith methodsFor: 'server locks'!
{Lock} newLock: clubID {ID | NULL}
	"Make a WallLock if clubID is NULL"
	clubID == NULL
		ifTrue: [^FeWallLockSmith make newLock: NULL]
		ifFalse: [^BooLock make: clubID with: self]!
*/
}
public FeBooLockSmith(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24593:FeBooLockSmith methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static boolean check(FeEdition edition) {
	Ravi.hack();
	return (edition.domain().isEqual((IntegerRegion.make(0, 3))))
	/* and: [((edition zoneOf: PrimSpec uInt8) domain
			isEqual: (IntegerRegion make: IntegerVarZero with: 3)) */
	&& (((PrimIntegerArray) ((FeArrayBundle) edition.retrieve().theOne()).array()).contentsEqual((UInt8Array.string("boo"))));
/*
udanax-top.st:24606:FeBooLockSmith class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	
	Ravi hack.
	^(edition domain isEqual: (IntegerRegion make: IntegerVarZero with: 3))
		"and: [((edition zoneOf: PrimSpec uInt8) domain
			isEqual: (IntegerRegion make: IntegerVarZero with: 3))"
		and: [((edition retrieve theOne cast: FeArrayBundle) array cast: PrimIntegerArray)
			contentsEqual: (UInt8Array string: 'boo')]"]"!
*/
}
public static FeBooLockSmith construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeBooLockSmith) (makeWrapper(edition));
/*
udanax-top.st:24615:FeBooLockSmith class methodsFor: 'private: wrapping'!
{FeBooLockSmith} construct: edition {FeEdition}
	
	self spec endorse: edition.
	^ (self makeWrapper: edition) cast: FeBooLockSmith!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeBooLockSmith(edition, spec());
/*
udanax-top.st:24620:FeBooLockSmith class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheBooLockSmithSpec = wrap;
/*
udanax-top.st:24624:FeBooLockSmith class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheBooLockSmithSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("BooLockSmith", "LockSmith", FE_BOO_LOCK_SMITH);
/*
udanax-top.st:24630:FeBooLockSmith class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'BooLockSmith'
		with: 'LockSmith'
		with: #FeBooLockSmith.!
*/
}
public static void linkTimeNonInherited() {
	TheBooLockSmithSpec = null;
/*
udanax-top.st:24636:FeBooLockSmith class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheBooLockSmithSpec := NULL.!
*/
}
public static FeBooLockSmith make() {
	return construct((FeEdition.fromArray((UInt8Array.string("boo")))));
/*
udanax-top.st:24642:FeBooLockSmith class methodsFor: 'pseudo constructors'!
{FeBooLockSmith CLIENT} make
	^self construct: (FeEdition fromArray: (UInt8Array string: 'boo'))!
*/
}
public static FeWrapperSpec spec() {
	return TheBooLockSmithSpec;
/*
udanax-top.st:24646:FeBooLockSmith class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheBooLockSmithSpec!
*/
}
public FeBooLockSmith() {
/*

Generated during transformation
*/
}
public FeBooLockSmith(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
