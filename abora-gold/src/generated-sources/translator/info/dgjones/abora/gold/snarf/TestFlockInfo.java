/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.TestFlockInfo;
import info.dgjones.abora.gold.snarf.TestPacker;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Used in conjunction with the TestPacker. Keeps a hash of the last contents that were
 * written to disk.
 */
public class TestFlockInfo extends FlockInfo {

	protected int myOldHash;
	protected int myPreviousHash;
	protected UInt8Array myOldContents;
/*
udanax-top.st:26841:
FlockInfo subclass: #TestFlockInfo
	instanceVariableNames: '
		myOldHash {UInt32}
		myPreviousHash {UInt32}
		myOldContents {UInt8Array}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:26848:
TestFlockInfo comment:
'Used in conjunction with the TestPacker. Keeps a hash of the last contents that were written to disk.'!
*/
/*
udanax-top.st:26850:
(TestFlockInfo getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:26901:
TestFlockInfo class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26904:
(TestFlockInfo getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TestFlockInfo.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public TestFlockInfo(Abraham shep, int snarfID, int index, int flags) {
	super(shep, snarfID, index, flags, 0);
	myOldHash = 0;
	myPreviousHash = 0;
	myOldContents = null;
/*
udanax-top.st:26855:TestFlockInfo methodsFor: 'create'!
create: shep {Abraham} 
	with: snarfID {SnarfID} 
	with: index {Int32} 
	with: flags {UInt32} 
	
	super
		create: shep
		with: snarfID
		with: index
		with: flags
		with: Int32Zero.
	myOldHash := UInt32Zero.
	myPreviousHash := UInt32Zero.
	myOldContents := NULL!
*/
}
public TestFlockInfo(Abraham shep, int snarfID, int index, int flags, int size) {
	super(shep, snarfID, index, flags, size);
	myOldHash = 0;
	myPreviousHash = 0;
	myOldContents = null;
/*
udanax-top.st:26870:TestFlockInfo methodsFor: 'create'!
create: shep {Abraham} 
	with: snarfID {SnarfID} 
	with: index {Int32} 
	with: flags {Int32} 
	with: size {Int32} 
	
	super
		create: shep
		with: snarfID
		with: index
		with: flags
		with: size.
	myOldHash := UInt32Zero.
	myPreviousHash := UInt32Zero.
	myOldContents := NULL!
*/
}
public void setContents(UInt8Array bits) {
	myOldContents = bits;
/*
udanax-top.st:26888:TestFlockInfo methodsFor: 'accessing'!
{void} setContents: bits {UInt8Array}
	myOldContents := bits!
*/
}
/**
 * Update the contents hash and other information from the current state of the shepherd.
 * Return true if the HASH only has changed since the last time.
 */
public boolean updateContentsInfo() {
	myPreviousHash = myOldHash;
	if (fetchShepherd() == null) {
		myOldHash = 0;
	}
	else {
		myOldHash = ((TestPacker) ((DiskManager) CurrentPacker.fluidGet())).computeHash(getShepherd());
	}
	return myPreviousHash != myOldHash;
/*
udanax-top.st:26891:TestFlockInfo methodsFor: 'accessing'!
{BooleanVar} updateContentsInfo
	"Update the contents hash and other information from the current state of the shepherd. Return true if the HASH only has changed since the last time."
	
	myPreviousHash := myOldHash.
	self fetchShepherd == NULL
		ifTrue: [myOldHash := UInt32Zero]
		ifFalse: [myOldHash := (CurrentPacker fluidGet cast: TestPacker) computeHash: self getShepherd].
	^myPreviousHash ~= myOldHash!
*/
}
/**
 * index = UInt32Zero assert: 'Should have index 0'.
 */
public static FlockInfo forgotten(Abraham shep, int snarfID, int index) {
	return new TestFlockInfo(shep, snarfID, index, FlockInfo.forgottenMask());
/*
udanax-top.st:26909:TestFlockInfo class methodsFor: 'pseudo constructors'!
{FlockInfo} forgotten: shep {Abraham} with: snarfID {SnarfID} with: index {Int32} 
	"index = UInt32Zero assert: 'Should have index 0'."
	^self create: shep
		with: snarfID
		with: index
		with: FlockInfo forgottenMask!
*/
}
/**
 * index = UInt32Zero assert: 'Should have index 0'.
 */
public static FlockInfo make(Abraham shep, int index) {
	return new TestFlockInfo(shep, 0, index, (((FlockInfo.contentsDirty() | FlockInfo.forgottenStateDirty()) & ~ FlockInfo.forgottenMask()) | FlockInfo.isNewMask()));
/*
udanax-top.st:26917:TestFlockInfo class methodsFor: 'pseudo constructors'!
{FlockInfo} make: shep {Abraham} with: index {IntegerVar} 
	"index = UInt32Zero assert: 'Should have index 0'."
	^self create: shep
		with: Int32Zero
		with: index DOTasLong 
		with: (((FlockInfo contentsDirty bitOr: FlockInfo forgottenStateDirty)
				bitAnd: FlockInfo forgottenMask bitInvert)
				bitOr: FlockInfo isNewMask)!
*/
}
/**
 * index = UInt32Zero assert: 'Should have index 0'.
 */
public static FlockInfo make(FlockInfo info, int snarfID, int index) {
	return new TestFlockInfo(info.getShepherd(), snarfID, index, (info.flags() & ~ FlockInfo.isNewMask()), info.oldSize());
/*
udanax-top.st:26926:TestFlockInfo class methodsFor: 'pseudo constructors'!
{FlockInfo} make: info {FlockInfo} with: snarfID {SnarfID} with: index {Int32} 
	"index = UInt32Zero assert: 'Should have index 0'."
	^self create: info getShepherd
		with: snarfID
		with: index
		with: (info flags bitAnd: FlockInfo isNewMask bitInvert)
		with: info oldSize!
*/
}
public static FlockInfo remembered(Abraham shep, int snarfID, int index) {
	if ( ! (index == 0)) {
		throw new AboraAssertionException("Should have index 0");
	}
	return new TestFlockInfo(shep, snarfID, index, 0);
/*
udanax-top.st:26934:TestFlockInfo class methodsFor: 'pseudo constructors'!
{FlockInfo} remembered: shep {Abraham} with: snarfID {SnarfID} with: index {Int32} 
	index = UInt32Zero assert: 'Should have index 0'.
	^self create: shep
		with: snarfID 
		with: index 
		with: UInt32Zero!
*/
}
public TestFlockInfo() {
/*

Generated during transformation
*/
}
public TestFlockInfo(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
