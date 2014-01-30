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
import info.dgjones.abora.gold.be.locks.MultiLock;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeMultiLockSmith;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Makes MultiLocks; see the comment there
 */
public class FeMultiLockSmith extends FeLockSmith {

	protected static FeWrapperSpec TheMultiLockSmithSpec;
/*
udanax-top.st:24861:
FeLockSmith subclass: #FeMultiLockSmith
	instanceVariableNames: ''
	classVariableNames: 'TheMultiLockSmithSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:24865:
FeMultiLockSmith comment:
'Makes MultiLocks; see the comment there'!
*/
/*
udanax-top.st:24867:
(FeMultiLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:24912:
FeMultiLockSmith class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:24915:
(FeMultiLockSmith getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeMultiLockSmith.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Lock newLock(ID clubID) {
	MuTable result;
	result = MuTable.make(SequenceSpace.make());
	TableStepper stomper = edition().stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Sequence name = (Sequence) stomper.position();
		FeEdition smith = (FeEdition) stomper.fetch();
		if (smith == null) {
			continue ;
		}
		result.introduce(name, (((FeLockSmith) (FeLockSmith.spec().wrap(smith))).newLock(clubID)));
	}
	stomper.destroy();
	return MultiLock.make(clubID, this, result.asImmuTable());
/*
udanax-top.st:24872:FeMultiLockSmith methodsFor: 'server locks'!
{Lock} newLock: clubID {ID | NULL}
	| result {MuTable of: Lock} |
	result := MuTable make: SequenceSpace make.
	self edition stepper forPositions: [ :name {Sequence} :smith {FeEdition} |
		result at: name
			introduce: (((FeLockSmith spec wrap: smith) cast: FeLockSmith) newLock: clubID)].
	^MultiLock make: clubID with: self with: result asImmuTable!
*/
}
/**
 * The named LockSmith
 */
public FeLockSmith lockSmith(Sequence name) {
	return (FeLockSmith) (FeLockSmith.spec().wrap(((FeEdition) (edition().get(name)))));
/*
udanax-top.st:24883:FeMultiLockSmith methodsFor: 'accessing'!
{FeLockSmith CLIENT} lockSmith: name {Sequence}
	"The named LockSmith"
	
	^(FeLockSmith spec wrap: ((self edition get: name) cast: FeEdition)) cast: FeLockSmith!
*/
}
/**
 * The names of all the Locksmiths this uses.
 */
public SequenceRegion lockSmithNames() {
	return (SequenceRegion) edition().domain();
/*
udanax-top.st:24888:FeMultiLockSmith methodsFor: 'accessing'!
{SequenceRegion CLIENT of: Sequence} lockSmithNames
	"The names of all the Locksmiths this uses."
	
	^self edition domain cast: SequenceRegion!
*/
}
/**
 * Add or change a LockSmith
 */
public FeMultiLockSmith with(Sequence name, FeLockSmith smith) {
	return (FeMultiLockSmith) (FeMultiLockSmith.construct((edition().with(name, smith.edition()))));
/*
udanax-top.st:24893:FeMultiLockSmith methodsFor: 'accessing'!
{FeMultiLockSmith CLIENT} with: name {Sequence} with: smith {FeLockSmith}
	"Add or change a LockSmith"
	
	^(FeMultiLockSmith construct: (self edition
		with: name
		with: smith edition)) cast: FeMultiLockSmith!
*/
}
/**
 * Add or change a LockSmith
 */
public FeMultiLockSmith without(Sequence name) {
	return (FeMultiLockSmith) (FeMultiLockSmith.construct((edition().without(name))));
/*
udanax-top.st:24900:FeMultiLockSmith methodsFor: 'accessing'!
{FeMultiLockSmith CLIENT} without: name {Sequence}
	"Add or change a LockSmith"
	
	^(FeMultiLockSmith construct: (self edition without: name)) cast: FeMultiLockSmith!
*/
}
public FeMultiLockSmith(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:24907:FeMultiLockSmith methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public static boolean check(FeEdition edition) {
	return (SequenceSpace.make().isEqual(edition.coordinateSpace())) && (FeWrapper.checkSubEditions(edition, edition.domain(), FeLockSmith.spec(), true));
/*
udanax-top.st:24920:FeMultiLockSmith class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	
	^(SequenceSpace make isEqual: edition coordinateSpace)
		and: [FeWrapper checkSubEditions: edition
			with: edition domain
			with: FeLockSmith spec
			with: true]!
*/
}
public static FeMultiLockSmith construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeMultiLockSmith) (makeWrapper(edition));
/*
udanax-top.st:24928:FeMultiLockSmith class methodsFor: 'private: wrapping'!
{FeMultiLockSmith} construct: edition {FeEdition}
	
	self spec endorse: edition.
	^ (self makeWrapper: edition) cast: FeMultiLockSmith!
*/
}
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeMultiLockSmith(edition, spec());
/*
udanax-top.st:24933:FeMultiLockSmith class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheMultiLockSmithSpec = wrap;
/*
udanax-top.st:24937:FeMultiLockSmith class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheMultiLockSmithSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("MultiLockSmith", "LockSmith", FE_MULTI_LOCK_SMITH);
/*
udanax-top.st:24943:FeMultiLockSmith class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'MultiLockSmith'
		with: 'LockSmith'
		with: #FeMultiLockSmith.!
*/
}
public static void linkTimeNonInherited() {
	TheMultiLockSmithSpec = null;
/*
udanax-top.st:24949:FeMultiLockSmith class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheMultiLockSmithSpec := NULL.!
*/
}
public static FeMultiLockSmith make() {
	return construct((FeEdition.empty(SequenceSpace.make())));
/*
udanax-top.st:24955:FeMultiLockSmith class methodsFor: 'pseudo constructors'!
{FeMultiLockSmith CLIENT} make
	^self construct: (FeEdition empty: SequenceSpace make)!
*/
}
public static FeWrapperSpec spec() {
	return TheMultiLockSmithSpec;
/*
udanax-top.st:24959:FeMultiLockSmith class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheMultiLockSmithSpec!
*/
}
/**
 * {FeLockSmith CLIENT} lockSmith: name {Sequence}
 * {SequenceRegion CLIENT of: Sequence} lockSmithNames
 * {FeMultiLockSmith CLIENT} with: name {Sequence} with: smith {FeLockSmith}
 * {FeMultiLockSmith CLIENT} without: name {Sequence}
 */
public static void infostProtocol() {
/*
udanax-top.st:24965:FeMultiLockSmith class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeLockSmith CLIENT} lockSmith: name {Sequence}
{SequenceRegion CLIENT of: Sequence} lockSmithNames
{FeMultiLockSmith CLIENT} with: name {Sequence} with: smith {FeLockSmith}
{FeMultiLockSmith CLIENT} without: name {Sequence}
"!
*/
}
public FeMultiLockSmith() {
/*

Generated during transformation
*/
}
public FeMultiLockSmith(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
