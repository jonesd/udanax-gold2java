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
import info.dgjones.abora.gold.be.locks.WallLock;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Makes WallLocks; see the comment there
 */
public class FeWallLockSmith extends FeLockSmith {

	protected static FeWrapperSpec TheWallLockSmithSpec;
/*
udanax-top.st:24972:
FeLockSmith subclass: #FeWallLockSmith
	instanceVariableNames: ''
	classVariableNames: 'TheWallLockSmithSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:24976:
FeWallLockSmith comment:
'Makes WallLocks; see the comment there'!
*/
/*
udanax-top.st:24978:
(FeWallLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24994:
FeWallLockSmith class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24997:
(FeWallLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWallLockSmith.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Lock newLock(ID clubID) {
	return WallLock.make(clubID, this);
/*
udanax-top.st:24983:FeWallLockSmith methodsFor: 'server locks'!
{Lock} newLock: clubID {ID | NULL}
	^WallLock make: clubID with: self!
*/
}
public FeWallLockSmith(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24989:FeWallLockSmith methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static boolean check(FeEdition edition) {
	Ravi.hack();
	return (edition.domain().isEqual((IntegerRegion.make(0, 4))))
	/* and: [((edition zoneOf: PrimSpec uInt8) domain
			isEqual: (IntegerRegion make: IntegerVarZero with: 4)) */
	&& (((PrimIntegerArray) ((FeArrayBundle) edition.retrieve().theOne()).array()).contentsEqual((UInt8Array.string("wall"))));
/*
udanax-top.st:25002:FeWallLockSmith class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	
	Ravi hack.
	^(edition domain isEqual: (IntegerRegion make: IntegerVarZero with: 4))
		"and: [((edition zoneOf: PrimSpec uInt8) domain
			isEqual: (IntegerRegion make: IntegerVarZero with: 4))"
		and: [((edition retrieve theOne cast: FeArrayBundle) array cast: PrimIntegerArray)
			contentsEqual: (UInt8Array string: 'wall')]"]"!
*/
}
public static FeWallLockSmith construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeWallLockSmith) (makeWrapper(edition));
/*
udanax-top.st:25011:FeWallLockSmith class methodsFor: 'private: wrapping'!
{FeWallLockSmith} construct: edition {FeEdition}
	
	self spec endorse: edition.
	^ (self makeWrapper: edition) cast: FeWallLockSmith!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeWallLockSmith(edition, spec());
/*
udanax-top.st:25016:FeWallLockSmith class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheWallLockSmithSpec = wrap;
/*
udanax-top.st:25020:FeWallLockSmith class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheWallLockSmithSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("WallLockSmith", "LockSmith", FE_WALL_LOCK_SMITH);
/*
udanax-top.st:25026:FeWallLockSmith class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'WallLockSmith'
		with: 'LockSmith'
		with: #FeWallLockSmith.!
*/
}
public static void linkTimeNonInherited() {
	TheWallLockSmithSpec = null;
/*
udanax-top.st:25032:FeWallLockSmith class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheWallLockSmithSpec := NULL.!
*/
}
public static FeWallLockSmith make() {
	return construct((FeEdition.fromArray((UInt8Array.string("wall")))));
/*
udanax-top.st:25038:FeWallLockSmith class methodsFor: 'pseudo constructors'!
{FeWallLockSmith CLIENT} make
	^self construct: (FeEdition fromArray: (UInt8Array string: 'wall'))!
*/
}
public static FeWrapperSpec spec() {
	return TheWallLockSmithSpec;
/*
udanax-top.st:25042:FeWallLockSmith class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheWallLockSmithSpec!
*/
}
public FeWallLockSmith() {
/*

Generated during transformation
*/
}
public FeWallLockSmith(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
