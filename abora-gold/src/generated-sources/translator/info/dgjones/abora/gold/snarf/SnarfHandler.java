/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.IntArray;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.urdi.SnarfHandle;
import info.dgjones.abora.gold.snarf.FlockLocation;
import info.dgjones.abora.gold.snarf.SnarfHandler;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A SnarfHandler breaks a snarf into abstract subarrays of bytes into whic flocks are
 * stored.  These indexed flock storage areas are accessed through readStreams and
 * writeStreams provided by the SnarfHandler.  SnarfHandlers also provide the ability to
 * resize these flock areas and associate a couple of flag bits with them.  All access to the
 * snarf goes through a single snarfHandler.
 * The beginning of the snarf is dedicated to a table that describes the locations and sizes
 * of the contained flock areas.  Currently, we allocate space between the flock nearest the
 * front of the snarf and the end of the mapTable.  When not enough space exists between the
 * two, we compact the flock storage areas towards the back (highest address) of the snarf
 * and try to allocate again.
 * An index in the snarfHAndler can be associated either with one of these flock storage
 * areas or with a snarfID and index to look further for the storage of a given flock.  Right
 * now, the SnarfHAndler keeps the forwarding information in a flock storage area, but it
 * will soon be put into the mapTable directly.
 * Forwarding pointers occur when a flock outgrows a snarf, and must be moved elsewhere.
 * Eventually all other snarfs that have objects which point to the forwarding pointer are
 * updated, and the forwarding pointer can be deallocated, but decisions about this must be
 * made by objects external to the SnarfHandler.
 * The forwarded flag is stored on the snarfID.  The forgotten flag is stored on the size.
 * Both use the same Flag mask for accessing the flag, and the Value mask for accessing the
 * value.
 */
public class SnarfHandler extends Heaper {

	protected SnarfHandle myHandle;
	protected int myMapCount;
	protected int mySpaceLeft;
	protected int myNearest;
	protected static int Flag;
	protected static int SizeOffset;
	protected static boolean UseFences;
	protected static int Value;
/*
udanax-top.st:51525:
Heaper subclass: #SnarfHandler
	instanceVariableNames: '
		myHandle {SnarfHandle star}
		myMapCount {Int4}
		mySpaceLeft {Int4}
		myNearest {Int4}'
	classVariableNames: '
		Flag {UInt4} 
		SizeOffset {Int4} 
		UseFences {BooleanVar} 
		Value {UInt4} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:51537:
SnarfHandler comment:
'A SnarfHandler breaks a snarf into abstract subarrays of bytes into whic flocks are stored.  These indexed flock storage areas are accessed through readStreams and writeStreams provided by the SnarfHandler.  SnarfHandlers also provide the ability to resize these flock areas and associate a couple of flag bits with them.  All access to the snarf goes through a single snarfHandler.
The beginning of the snarf is dedicated to a table that describes the locations and sizes of the contained flock areas.  Currently, we allocate space between the flock nearest the front of the snarf and the end of the mapTable.  When not enough space exists between the two, we compact the flock storage areas towards the back (highest address) of the snarf and try to allocate again.
An index in the snarfHAndler can be associated either with one of these flock storage areas or with a snarfID and index to look further for the storage of a given flock.  Right now, the SnarfHAndler keeps the forwarding information in a flock storage area, but it will soon be put into the mapTable directly.
Forwarding pointers occur when a flock outgrows a snarf, and must be moved elsewhere.  Eventually all other snarfs that have objects which point to the forwarding pointer are updated, and the forwarding pointer can be deallocated, but decisions about this must be made by objects external to the SnarfHandler.
The forwarded flag is stored on the snarfID.  The forgotten flag is stored on the size.  Both use the same Flag mask for accessing the flag, and the Value mask for accessing the value.'!
*/
/*
udanax-top.st:51547:
(SnarfHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:51932:
SnarfHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:51935:
(SnarfHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SnarfHandler.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * If the flock specified by index has been forwarded, return a FlockLocation with the
 * SnarfID and index of its new location.
 */
public FlockLocation fetchForward(int index) {
	checkIndex(index);
	if (isForwarded(index)) {
		/* Forwarded.  The info is stored in the mapCell. */
		return FlockLocation.make((getOffset(index)), (getSize(index)));
	}
	return null;
/*
udanax-top.st:51552:SnarfHandler methodsFor: 'reading'!
{FlockLocation | NULL} fetchForward: index {Int32}
	"If the flock specified by index has been forwarded, return a FlockLocation with the SnarfID and index of its new location."
	self checkIndex: index.
	(self isForwarded: index) ifTrue:
		["Forwarded.  The info is stored in the mapCell."
		^FlockLocation make: (self getOffset: index) with: (self getSize: index)].
	^NULL!
*/
}
/**
 * Return the number of bytes in the flock at index
 */
public int flockSize(int index) {
	return ((myHandle.get32((mapCellOffset(index)) + SizeOffset)) & Value) - (SnarfHandler.fenceSize() * 2);
/*
udanax-top.st:51561:SnarfHandler methodsFor: 'reading'!
{Int32} flockSize: index {Int32}
	"Return the number of bytes in the flock at index"
	
	^((myHandle get32: (self mapCellOffset: index) + SizeOffset) bitAnd: Value) - (SnarfHandler fenceSize * 2)!
*/
}
/**
 * The forgotten flag is the flag bit associated with each flock.  It is set when the
 * flock has been forgotten, which means that there are no more persistent pointers
 * to the flock.  When a flock is forgotten AND is not in RAM, the SnarfPacker is
 * free to bring the flock back into RAM and destroy it, which deletes it from the snarf.
 * Return true if the forgotten flag has been set for the flock at index.
 */
public boolean isForgotten(int index) {
	return (Flag & (myHandle.get32((mapCellOffset(index)) + SizeOffset))) == Flag;
/*
udanax-top.st:51566:SnarfHandler methodsFor: 'reading'!
{BooleanVar} isForgotten: index {Int32}
	"The forgotten flag is the flag bit associated with each flock.  It is set when the
	flock has been forgotten, which means that there are no more persistent pointers
	to the flock.  When a flock is forgotten AND is not in RAM, the SnarfPacker is
	free to bring the flock back into RAM and destroy it, which deletes it from the snarf.
	 
	 Return true if the forgotten flag has been set for the flock at index."
	
	^(Flag bitAnd: (myHandle get32: (self mapCellOffset: index) + SizeOffset)) == Flag!
*/
}
/**
 * Return true if there's a flock or forwarder at index.
 */
public boolean isOccupied(int index) {
	return index >= 0 && (index < myMapCount && ((isForwarded(index)) || ((getSize(index)) > 0)));
/*
udanax-top.st:51576:SnarfHandler methodsFor: 'reading'!
{BooleanVar} isOccupied: index {Int32}
	"Return true if there's a flock or forwarder at index."
	
	^index >= Int32Zero and: [index < myMapCount and: [(self isForwarded: index) or: [(self getSize: index) > Int32Zero]]]!
*/
}
/**
 * Return the number of slots allocated in the map table.
 */
public int mapCount() {
	return myMapCount;
/*
udanax-top.st:51581:SnarfHandler methodsFor: 'reading'!
{Int32} mapCount
	"Return the number of slots allocated in the map table."
	
	^myMapCount!
*/
}
/**
 * Return a stream on the area of the snarf allocated to mapIndex.
 * This stream must be used immediately, then thrown away.
 */
public XnReadStream readStream(int index) {
	checkIndex(index);
	if (isForwarded(index)) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_AFLOCK);
	}
	return XnReadStream.make(myHandle.getDataP(), (flockOffset(index)), (flockSize(index)));
/*
udanax-top.st:51586:SnarfHandler methodsFor: 'reading'!
{XnReadStream} readStream: index {Int32}
	"Return a stream on the area of the snarf allocated to mapIndex.  
	 This stream must be used immediately, then thrown away."
	self checkIndex: index.
	(self isForwarded: index) ifTrue: [Heaper BLAST: #MustBeAFlock].
	^XnReadStream
		make: myHandle getDataP
		with: (self flockOffset: index)
		with: (self flockSize: index)!
*/
}
/**
 * Return the snarfID of the snarf this handle holds.
 */
public int snarfID() {
	return myHandle.getSnarfID();
/*
udanax-top.st:51597:SnarfHandler methodsFor: 'reading'!
{SnarfID} snarfID
	"Return the snarfID of the snarf this handle holds."
	^myHandle getSnarfID!
*/
}
/**
 * Return the amount space left in the snarf.
 */
public int spaceLeft() {
	return mySpaceLeft;
/*
udanax-top.st:51601:SnarfHandler methodsFor: 'reading'!
{Int32} spaceLeft
	"Return the amount space left in the snarf."
	
	^mySpaceLeft!
*/
}
/**
 * Add more cells to the mapTable.  Make sure that there is enough space for
 * those cells, then initialize.  The size is initially 0 and the offset points past
 * the end of the snarf.
 */
public void allocateCells(int indices) {
	int newCells;
	int space;
	newCells = indices;
	if (newCells <= 0) {
		return ;
	}
	space = newCells * SnarfHandler.mapCellSize();
	clearSpace(space);
	myMapCount = myMapCount + newCells;
	mySpaceLeft = mySpaceLeft - space;
	for (int index = myMapCount - newCells; index < myMapCount; index ++ ) {
		/* Zero all the counts, just like wipeFlock. */
		myHandle.put32((mapCellOffset(index)) + SizeOffset, 0);
		storeIndex(index, flocksEnd());
	}
	consistencyCheck();
	checkFences();
/*
udanax-top.st:51608:SnarfHandler methodsFor: 'writing'!
{void} allocateCells: indices {IntegerVar}
	"Add more cells to the mapTable.  Make sure that there is enough space for
	 those cells, then initialize.  The size is initially 0 and the offset points past 
	 the end of the snarf."
	
	| newCells {Int32} space {Int32} |
	newCells _ indices DOTasLong.
	newCells <= Int32Zero ifTrue: [^VOID].
	space _ newCells * SnarfHandler mapCellSize.
	self clearSpace: space.
	myMapCount _ myMapCount + newCells.
	mySpaceLeft _ mySpaceLeft - space.
	myMapCount-newCells almostTo: myMapCount do: [:index {Int32} |
		"Zero all the counts, just like wipeFlock."
		myHandle at: (self mapCellOffset: index) + SizeOffset put32: Int32Zero.
		self at: index storeIndex: self flocksEnd].
	self consistencyCheck.
	self checkFences!
*/
}
/**
 * Allocate flockSize bytes for the flock at the index ind.
 */
public void allocate(int ind, int flockSize) {
	int index;
	int size;
	if ( ! (flockSize > 0)) {
		throw new AboraAssertionException("Must allocate some space");
	}
	size = flockSize + (SnarfHandler.fenceSize() * 2);
	index = ind;
	checkIndex(index);
	clearSpace(size);
	if ( ! (isForwarded(index))) {
		mySpaceLeft = mySpaceLeft + (getSize(index));
	}
	mySpaceLeft = mySpaceLeft - size;
	storeIndex(index, nearestFlock() - size);
	storeSize(index, size);
	mendFences(index);
	consistencyCheck();
	checkFences();
/*
udanax-top.st:51627:SnarfHandler methodsFor: 'writing'!
{void} at: ind {IntegerVar} allocate: flockSize {Int32}
	"Allocate flockSize bytes for the flock at the index ind."
	
	| index {Int32} size {Int32} |
	flockSize > Int32Zero assert: 'Must allocate some space'.
	size _ flockSize + (SnarfHandler fenceSize * 2).
	index _ ind DOTasLong.
	self checkIndex: index.
	self clearSpace: size.
	(self isForwarded: index) ifFalse: [mySpaceLeft _ mySpaceLeft + (self getSize: index)].
	mySpaceLeft _ mySpaceLeft - size.
	self at: index storeIndex: self nearestFlock - size.
	self at: index storeSize: size.
	self mendFences: index.
	self consistencyCheck.
	self checkFences!
*/
}
/**
 * See the comment on isForgotten:.  Set or clear the forgetFlag for the flock at index.
 */
public void storeForget(int index, boolean flag) {
	int offset;
	checkIndex(index);
	offset = (mapCellOffset(index)) + SizeOffset;
	/* Keep everything else the same. */
	if (flag) {
		myHandle.put32(offset, (Flag | (myHandle.get32(offset))));
	}
	else {
		myHandle.put32(offset, (Value & (myHandle.get32(offset))));
	}
	checkFences();
/*
udanax-top.st:51644:SnarfHandler methodsFor: 'writing'!
{void} at: index {Int32} storeForget: flag {BooleanVar} 
	"See the comment on isForgotten:.  Set or clear the forgetFlag for the flock at index."
	| offset {Int32} |
	self checkIndex: index.
	offset _ (self mapCellOffset: index) + SizeOffset.
	"Keep everything else the same."
	flag ifTrue: [myHandle at: offset put32: (Flag bitOr: (myHandle get32: offset))]
		ifFalse: [myHandle at: offset put32: (Value bitAnd: (myHandle get32: offset))].
	self checkFences!
*/
}
/**
 * Associate a forwarder with index.  Throw away whatever storage
 * was assigned to it and store the forwarder information in the mapCell.
 */
public void forwardTo(int index, int newSnarfID, int newIndex) {
	wipeFlock(index);
	myHandle.put32((mapCellOffset(index)), (newSnarfID | Flag));
	myHandle.put32(((mapCellOffset(index)) + SizeOffset), (newIndex & Value));
/*
udanax-top.st:51655:SnarfHandler methodsFor: 'writing'!
{void} forward: index {IntegerVar} to: newSnarfID {SnarfID} with: newIndex {Int32}
	"Associate a forwarder with index.  Throw away whatever storage
	 was assigned to it and store the forwarder information in the mapCell."
	self wipeFlock: index.
	myHandle at: (self mapCellOffset: index DOTasLong) put32: (newSnarfID bitOr: Flag).
	myHandle at: ((self mapCellOffset: index DOTasLong) + SizeOffset) put32: (newIndex bitAnd: Value).!
*/
}
/**
 * Return true if I represent a writable snarf.
 */
public boolean isWritable() {
	return myHandle.isWritable();
/*
udanax-top.st:51663:SnarfHandler methodsFor: 'writing'!
{BooleanVar} isWritable
	"Return true if I represent a writable snarf. "
	
	^myHandle isWritable!
*/
}
/**
 * Make the handle for the receiver writable.
 */
public void makeWritable() {
	myHandle.makeWritable();
/*
udanax-top.st:51668:SnarfHandler methodsFor: 'writing'!
{void} makeWritable
	"Make the handle for the receiver writable."
	
	myHandle makeWritable!
*/
}
/**
 * Write out to the snarf any values held in instance variables (space
 * remaining, number of entries, etc.).
 */
public void rewrite() {
	myHandle.put32(0, myMapCount);
	myHandle.put32(SizeOffset, mySpaceLeft);
/*
udanax-top.st:51673:SnarfHandler methodsFor: 'writing'!
{void} rewrite
	"Write out to the snarf any values held in instance variables (space 
	remaining, number of entries, etc.)."
	
	myHandle at: Int32Zero put32: myMapCount.
	myHandle at: SizeOffset put32: mySpaceLeft!
*/
}
/**
 * Deallocate all space for the flock at index.  The slot for index remains however, and can
 * be reused for another flock.
 */
public void wipeFlock(int index) {
	checkIndex(index);
	if ( ! (isForwarded(index))) {
		mySpaceLeft = mySpaceLeft + (getSize(index));
	}
	myHandle.put32((mapCellOffset(index)) + SizeOffset, 0);
	storeIndex(index, flocksEnd());
	consistencyCheck();
	checkFences();
/*
udanax-top.st:51680:SnarfHandler methodsFor: 'writing'!
{void} wipeFlock: index {IntegerVar}
	"Deallocate all space for the flock at index.  The slot for index remains however, and can be reused for another flock."
	
	self checkIndex: index DOTasLong.
	(self isForwarded: index DOTasLong) ifFalse: [mySpaceLeft _ mySpaceLeft + (self getSize: index DOTasLong)].
	myHandle at: (self mapCellOffset: index DOTasLong) + SizeOffset put32: Int32Zero.
	self at: index DOTasLong storeIndex: self flocksEnd.
	self consistencyCheck.
	self checkFences!
*/
}
/**
 * Return a stream that can write into the bytes allocated to the flock at index.
 * The stream must be used immediately and thrown away.
 */
public XnWriteStream writeStream(int index) {
	checkIndex(index);
	if (isForwarded(index)) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_AFLOCK);
	}
	return XnWriteStream.make(myHandle.getData(), (flockOffset(index)), (flockSize(index)));
/*
udanax-top.st:51690:SnarfHandler methodsFor: 'writing'!
{XnWriteStream} writeStream: index {IntegerVar}
	"Return a stream that can write into the bytes allocated to the flock at index. 
	 The stream must be used immediately and thrown away."
	self checkIndex: index DOTasLong.
	(self isForwarded: index DOTasLong) ifTrue: [Heaper BLAST: #MustBeAFlock].
	^XnWriteStream
		make: myHandle getDataP
		with: (self flockOffset: index DOTasLong)
		with: (self flockSize: index DOTasLong)!
*/
}
/**
 * Put in the minimum necessary for a starting snarf.
 * All it needs is the number of objects and the spaceLeft.
 * This also writes the information to the real snarf.
 */
public void initializeSnarf() {
	myMapCount = 0;
	mySpaceLeft = flocksEnd() - SnarfHandler.mapOverhead();
	rewrite();
/*
udanax-top.st:51703:SnarfHandler methodsFor: 'initialize'!
{void} initializeSnarf
	"Put in the minimum necessary for a starting snarf.  
	 All it needs is the number of objects and the spaceLeft.
	 This also writes the information to the real snarf."
	 
	 myMapCount _ Int32Zero.
	 mySpaceLeft _ self flocksEnd - SnarfHandler mapOverhead.
	 self rewrite!
*/
}
/**
 * If we are using fences around flock storage areas, then return true only if the fences are
 * still in place for the flock at index.  Fences are extra storage at the front and back of
 * a flock storage area that contains the index of that flock.  These are used for runtime
 * checks that one flock hasn't stepped into the space of another.
 */
public boolean checkFence(int index) {
	if (UseFences) {
		int offset;
		int size;
		if (isForwarded(index)) {
			return true;
		}
		size = getSize(index);
		return size <= 0 || ((myHandle.get32((offset = getOffset(index)))) == index && ((myHandle.get32(offset + (getSize(index)) - SnarfHandler.fenceSize())) == index));
	}
	else {
		return true;
	}
/*
udanax-top.st:51714:SnarfHandler methodsFor: 'private: operations'!
{BooleanVar} checkFence: index {Int32} 
	"If we are using fences around flock storage areas, then return true only if the fences are still in place for the flock at index.  Fences are extra storage at the front and back of a flock storage area that contains the index of that flock.  These are used for runtime checks that one flock hasn't stepped into the space of another."
	
	UseFences
		ifTrue:
			[| offset {Int32} size {Int32} |
			(self isForwarded: index) ifTrue: [^true].
			size _ self getSize: index.
			^size <= Int32Zero
				or: [(myHandle get32: (offset _ self getOffset: index)) == index
					and: [(myHandle get32: offset + (self getSize: index) - SnarfHandler fenceSize) == index]]]
		ifFalse: [^true]!
*/
}
/**
 * See checkFence:  Check the fences for all flocks and blast if any are violated.
 */
public void checkFences() {
	/* Int32Zero to: myMapCount-1 do:
		[:i {Int32} | (self checkFence: i) ifFalse: [SnarfHandler BLAST: #BrokenFence]] */
/*
udanax-top.st:51727:SnarfHandler methodsFor: 'private: operations'!
{void} checkFences
	"See checkFence:  Check the fences for all flocks and blast if any are violated."
	
	"Int32Zero to: myMapCount-1 do:
		[:i {Int32} | (self checkFence: i) ifFalse: [SnarfHandler BLAST: #BrokenFence]]"!
*/
}
/**
 * Blast if the index is not represented in the table.  This is just simple bounds checking.
 */
public void checkIndex(int index) {
	if (index >= myMapCount && (index >= 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
/*
udanax-top.st:51733:SnarfHandler methodsFor: 'private: operations'!
{void} checkIndex: index {Int32}
	"Blast if the index is not represented in the table.  This is just simple bounds checking."
	
	(index >= myMapCount and: [index >= Int32Zero]) ifTrue: [MuTable BLAST: #NotInTable]!
*/
}
/**
 * This checks for count bytes available at the end of the mapTable.  If
 * there isn't enough, it compacts everything and tries again.
 */
public void clearSpace(int count) {
	consistencyCheck();
	if (nearestFlock() < (mapEnd() + count)) {
		recomputeNearest();
		if (nearestFlock() < (mapEnd() + count)) {
			compact();
			if ( ! (nearestFlock() >= (mapEnd() + count))) {
				throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_ROOM);
			}
		}
	}
/*
udanax-top.st:51738:SnarfHandler methodsFor: 'private: operations'!
{void} clearSpace: count {Int32}
	"This checks for count bytes available at the end of the mapTable.  If
	 there isn't enough, it compacts everything and tries again."
	self consistencyCheck.
	self nearestFlock < (self mapEnd + count) ifTrue: 
		[self recomputeNearest.
		self nearestFlock < (self mapEnd + count) ifTrue: 
			[self compact.
			self nearestFlock >= (self mapEnd + count)
				ifFalse: [Heaper BLAST: #MustHaveRoom]]]!
*/
}
/**
 * Compress flock storage areas towards the end of the snarf, leaving all
 * freespace between the end of the mapTable and the nearest flock.
 */
public void compact() {
	int sweeper;
	Int32Array offsets;
	Int32Array indices;
	checkFences();
	sweeper = flocksEnd();
	myNearest = sweeper;
	/* Load up all the offset into an array.  Make cells that are forwarded just point past the end of the snarf. */
	offsets = Int32Array.make(myMapCount + 1);
	for (int i = 0; i < myMapCount; i ++ ) {
		if (isForwarded(i)) {
			offsets.storeUInt(i, sweeper);
		}
		else {
			offsets.storeUInt(i, (getOffset(i)));
		}
	}
	offsets.storeUInt(myMapCount, 0);
	indices = SnarfHandler.sort(offsets);
	for (int i2 = 0; i2 < myMapCount; i2 ++ ) {
		int indexToMove;
		int offsetToMove;
		int count;
		indexToMove = indices.uIntAt(i2);
		offsetToMove = offsets.uIntAt(i2);
		if (offsetToMove < sweeper) {
			count = getSize(indexToMove);
			sweeper = sweeper - count;
			myHandle.moveBytes(offsetToMove, sweeper, count);
			/* This storeIndex will also push myNearest. */
			storeIndex(indexToMove, sweeper);
		}
	}
	checkFences();
	offsets.destroy();
/*
udanax-top.st:51750:SnarfHandler methodsFor: 'private: operations'!
{void} compact
	"Compress flock storage areas towards the end of the snarf, leaving all
	 freespace between the end of the mapTable and the nearest flock."
	
	| sweeper {Int32} offsets {UInt32Array} indices {UInt32Array} | 
	self checkFences. 
	sweeper _ self flocksEnd.
	myNearest _ sweeper.
	"Load up all the offset into an array.  Make cells that are forwarded just point past the end of the snarf."
	offsets _ UInt32Array make: myMapCount + 1.
	Int32Zero almostTo: myMapCount do:
		[ :i {Int32} |
		(self isForwarded: i)
			ifTrue: [offsets at: i storeUInt: sweeper]
			ifFalse: [offsets at: i storeUInt: (self getOffset: i)]].
	offsets at: myMapCount storeUInt: UInt32Zero.
	indices _ SnarfHandler sort: offsets.
	Int32Zero almostTo: myMapCount do:
		[:i2 {Int32} |
		| indexToMove {Int32} offsetToMove {Int32} count {Int32} |
		indexToMove _ indices uIntAt: i2.
		offsetToMove _ offsets uIntAt: i2.
		offsetToMove < sweeper ifTrue:
			[count _ self getSize: indexToMove.
			sweeper _ sweeper - count.
			myHandle moveBytes: offsetToMove with: sweeper with: count.
			"This storeIndex will also push myNearest."
			self at: indexToMove storeIndex: sweeper]].
	self checkFences.
	offsets destroy.!
*/
}
/**
 * Generic checking hook to do slow runtime consistency checking when debugging.  No checks
 * are active currently.
 */
public void consistencyCheck() {
	/* self compact.
	mySpaceLeft == (self nearestFlock - self mapEnd) assert: 'space mismatch'. */
	/* | sum {Int32} |
	sum _ Int32Zero.
	Int32Zero almostTo: myMapCount do: 
		[:i {Int32} |
		(self isForwarded: i) ifFalse: [sum _ sum + (self getSize: i)]].
	sum + self mapEnd + mySpaceLeft == myHandle getDataSize assert: 'Space difference' */
/*
udanax-top.st:51781:SnarfHandler methodsFor: 'private: operations'!
{void} consistencyCheck
	"Generic checking hook to do slow runtime consistency checking when debugging.  No checks are active currently."
	"self compact.
	mySpaceLeft == (self nearestFlock - self mapEnd) assert: 'space mismatch'."
	"| sum {Int32} |
	sum _ Int32Zero.
	Int32Zero almostTo: myMapCount do: 
		[:i {Int32} |
		(self isForwarded: i) ifFalse: [sum _ sum + (self getSize: i)]].
	sum + self mapEnd + mySpaceLeft == myHandle getDataSize assert: 'Space difference'"!
*/
}
/**
 * Couldn't resist the name.  Set up the fences for the flock at index.  See checkFence:
 */
public void mendFences(int index) {
	if (UseFences) {
		int offset;
		offset = getOffset(index);
		myHandle.put32(offset, index);
		myHandle.put32(offset + (getSize(index)) - SnarfHandler.fenceSize(), index);
	}
/*
udanax-top.st:51793:SnarfHandler methodsFor: 'private: operations'!
{void} mendFences: index {Int32}
	"Couldn't resist the name.  Set up the fences for the flock at index.  See checkFence:"
	
	UseFences ifTrue:
		[| offset {Int32} |
		offset _ self getOffset: index.
		myHandle at: offset put32: index.
		myHandle at: offset + (self getSize: index) - SnarfHandler fenceSize put32: index]!
*/
}
/**
 * Return the location of the nearest flock. Everything between the
 * end of the map and the nearest flock is free space. We normally
 * allocate everything from the back of the snarf forward. When we
 * run out of enough contiguous space, we simply compact.
 * We keep a cache of the current nearest flock.  The cache maintins the invariant that it
 * *must* point to an offset less than or equal to the nearestFlock.  Thus it can be too
 * close
 * to the mapTable, in which case we will recompute it from scratch.
 */
public int nearestFlock() {
	if (myNearest == 0) {
		recomputeNearest();
	}
	return myNearest;
/*
udanax-top.st:51802:SnarfHandler methodsFor: 'private: operations'!
{Int32} nearestFlock
	"Return the location of the nearest flock. Everything between the 
	end of the map and the nearest flock is free space. We normally 
	allocate everything from the back of the snarf forward. When we 
	run out of enough contiguous space, we simply compact.
	
	We keep a cache of the current nearest flock.  The cache maintins the invariant that it
	 *must* point to an offset less than or equal to the nearestFlock.  Thus it can be too close 
	 to the mapTable, in which case we will recompute it from scratch."
	myNearest == Int32Zero ifTrue: [self recomputeNearest].
	^myNearest!
*/
}
/**
 * Recalculate the nearest flock by looking at the start of every flock and taking the min.
 */
public void recomputeNearest() {
	myNearest = flocksEnd();
	for (int index = 0; index < myMapCount; index ++ ) {
		if ( ! (isForwarded(index)) && ((getSize(index)) > 0)) {
			int offset;
			offset = getOffset(index);
			if (offset < myNearest) {
				myNearest = offset;
			}
		}
	}
/*
udanax-top.st:51815:SnarfHandler methodsFor: 'private: operations'!
{void} recomputeNearest
	"Recalculate the nearest flock by looking at the start of every flock and taking the min."
	myNearest _ self flocksEnd.
	Int32Zero almostTo: myMapCount do: 
		[:index {Int32} |
		((self isForwarded: index) not and: [(self getSize: index) > Int32Zero]) ifTrue: 
			[| offset {Int32} |
			offset _ self getOffset: index.
			offset < myNearest ifTrue: [myNearest _ offset]]]!
*/
}
/**
 * Store the offset as the starting location for the data of the flock at index.
 * Update the cache of nearestFlock.  This also clears the forwarded flag.
 */
public void storeIndex(int index, int offset) {
	if (offset < myNearest) {
		myNearest = offset;
	}
	myHandle.put32((mapCellOffset(index)), (offset & Value));
/*
udanax-top.st:51828:SnarfHandler methodsFor: 'private: layout'!
{void} at: index {Int32} storeIndex: offset {Int32}
	"Store the offset as the starting location for the data of the flock at index.  
	 Update the cache of nearestFlock.  This also clears the forwarded flag."
	
	offset < myNearest ifTrue: [myNearest _ offset].
	myHandle at: (self mapCellOffset: index) put32: (offset bitAnd: Value)!
*/
}
/**
 * Store size as the number of bytes for the flock at index.  If the
 * space is at a 0, then change the corresponding pointer to past the end of
 * the snarf so that we don't find it in our searches.
 */
public void storeSize(int index, int size) {
	int offset;
	offset = (mapCellOffset(index)) + SizeOffset;
	/* Keep the old flags. */
	myHandle.put32(offset, ((size & Value) | ((myHandle.get32(offset)) & Flag)));
	if (size == 0) {
		storeIndex(index, flocksEnd());
	}
/*
udanax-top.st:51835:SnarfHandler methodsFor: 'private: layout'!
{void} at: index {Int32} storeSize: size {Int32}
	"Store size as the number of bytes for the flock at index.  If the 
	 space is at a 0, then change the corresponding pointer to past the end of 
	 the snarf so that we don't find it in our searches."
	
	| offset {Int32} |
	offset _ (self mapCellOffset: index) + SizeOffset.
	"Keep the old flags."
	myHandle at: offset put32: ((size bitAnd: Value) bitOr: ((myHandle get32: offset) bitAnd: Flag)).
	size == Int32Zero ifTrue: [self at: index storeIndex: self flocksEnd]!
*/
}
/**
 * Return the index of the first byte of the actual data associated with flock number index.
 * This is like indexOf: except that it leaves room for fencePosts on either side of the
 * flock storage area.
 */
public int flockOffset(int index) {
	return ((myHandle.get32((mapCellOffset(index)))) & Value) + SnarfHandler.fenceSize();
/*
udanax-top.st:51846:SnarfHandler methodsFor: 'private: layout'!
{Int32} flockOffset: index {Int32}
	"Return the index of the first byte of the actual data associated with flock number index.  This is like indexOf: except that it leaves room for fencePosts on either side of the flock storage area."
	
	^((myHandle get32: (self mapCellOffset: index)) bitAnd: Value) + SnarfHandler fenceSize!
*/
}
/**
 * Return the index of the cell one greater than the size of the entire snarf.  This is just
 * past the end of the storage area for flocks.
 */
public int flocksEnd() {
	return myHandle.getDataSize();
/*
udanax-top.st:51851:SnarfHandler methodsFor: 'private: layout'!
{Int32} flocksEnd
	"Return the index of the cell one greater than the size of the entire snarf.  This is just past the end of the storage area for flocks."
	
	^myHandle getDataSize!
*/
}
/**
 * Return the index of the first byte of the actual data associated with
 * flock number index.  This area includes space for fencePosts and whatever
 * other things we might dream up that go with the flock in its storage area.
 */
public int getOffset(int index) {
	int offset;
	offset = myHandle.get32((mapCellOffset(index)));
	return offset & Value;
/*
udanax-top.st:51856:SnarfHandler methodsFor: 'private: layout'!
{Int32} getOffset: index {Int32}
	"Return the index of the first byte of the actual data associated with
	 flock number index.  This area includes space for fencePosts and whatever 
	 other things we might dream up that go with the flock in its storage area."
	
	| offset {Int32} |
	offset _ myHandle get32: (self mapCellOffset: index).
	^offset bitAnd: Value!
*/
}
/**
 * Return the number of bytes in the flock at index.  This includes space allocated
 * internally for fencePosts and the like.
 */
public int getSize(int index) {
	int size;
	size = (myHandle.get32((mapCellOffset(index)) + SizeOffset)) & Value;
	return size;
/*
udanax-top.st:51865:SnarfHandler methodsFor: 'private: layout'!
{Int32} getSize: index {Int32}
	"Return the number of bytes in the flock at index.  This includes space allocated internally for fencePosts and the like."
	
	| size {Int32} |
	size _ (myHandle get32: (self mapCellOffset: index) + SizeOffset) bitAnd: Value.
	^size!
*/
}
/**
 * Return the internal bit that says whether the flock at index is represented by forwarding
 * information or by a flock area
 */
public boolean isForwarded(int index) {
	return (Flag & (myHandle.get32((mapCellOffset(index))))) == Flag;
/*
udanax-top.st:51872:SnarfHandler methodsFor: 'private: layout'!
{BooleanVar} isForwarded: index {Int32}
	"Return the internal bit that says whether the flock at index is represented by forwarding information or by a flock area"
	
	^(Flag bitAnd: (myHandle get32: (self mapCellOffset: index))) == Flag!
*/
}
/**
 * Return the offset into the snarf for the mapCell that has the data for the flock at index.
 */
public int mapCellOffset(int index) {
	return (SnarfHandler.mapCellSize() * index) + SnarfHandler.mapOverhead();
/*
udanax-top.st:51877:SnarfHandler methodsFor: 'private: layout'!
{Int32 INLINE} mapCellOffset: index {Int32}
	"Return the offset into the snarf for the mapCell that has the data for the flock at index."
	
	^(SnarfHandler mapCellSize * index) + SnarfHandler mapOverhead!
*/
}
/**
 * Return the index of the cell just after the end of the map.  This is based on the number
 * of entries in the map.
 */
public int mapEnd() {
	return mapCellOffset(myMapCount);
/*
udanax-top.st:51882:SnarfHandler methodsFor: 'private: layout'!
{Int32} mapEnd
	"Return the index of the cell just after the end of the map.  This is based on the number of entries in the map."
	
	^self mapCellOffset: myMapCount!
*/
}
/**
 * Actually get from the snarf the number of map slots currently allocated,
 * including ones that are free for reuse. This is stored as the first thing in the
 * snarf.
 */
public int snarfMapCount() {
	return myHandle.get32(0);
/*
udanax-top.st:51887:SnarfHandler methodsFor: 'private: layout'!
{Int32} snarfMapCount
	"Actually get from the snarf the number of map slots currently allocated, 
	including ones that are free for reuse. This is stored as the first thing in the 
	snarf."
	^myHandle get32: Int32Zero!
*/
}
/**
 * Actually get from the snarf the amount of unallocated space remaining.
 */
public int snarfSpaceLeft() {
	return myHandle.get32(SizeOffset);
/*
udanax-top.st:51894:SnarfHandler methodsFor: 'private: layout'!
{Int32} snarfSpaceLeft
	"Actually get from the snarf the amount of unallocated space remaining."
	
	^myHandle get32: SizeOffset!
*/
}
/**
 * Write my internal constants to the snarf before I go away.
 */
public void destruct() {
	if (myHandle.isWritable()) {
		rewrite();
	}
	myHandle.destroy();
	myHandle = null;
	super.destruct();
/*
udanax-top.st:51901:SnarfHandler methodsFor: 'protected: destruct'!
{void} destruct
	"Write my internal constants to the snarf before I go away."
	myHandle isWritable ifTrue: [self rewrite].
	myHandle destroy.
	myHandle _ NULL.
	super destruct!
*/
}
public SnarfHandler(SnarfHandle handle) {
	super();
	if ( ! (handle != null)) {
		throw new AboraAssertionException("nil handle");
	}
	myHandle = handle;
	myMapCount = snarfMapCount();
	/* If I'm uninitialized, then generate the necessary data. */
	if (myMapCount == 0) {
		mySpaceLeft = flocksEnd() - SnarfHandler.mapOverhead();
	}
	else {
		mySpaceLeft = snarfSpaceLeft();
	}
	myNearest = 0;
/*
udanax-top.st:51910:SnarfHandler methodsFor: 'create'!
create: handle {SnarfHandle}
	super create.
	[handle ~~ nil assert: 'nil handle'] smalltalkOnly.
	myHandle _ handle.
	myMapCount _ self snarfMapCount.
	"If I'm uninitialized, then generate the necessary data."
	myMapCount == Int32Zero 
		ifTrue: [mySpaceLeft _ self flocksEnd - SnarfHandler mapOverhead]
		ifFalse: [mySpaceLeft _ self snarfSpaceLeft].
	myNearest _ Int32Zero!
*/
}
/*
udanax-top.st:51923:SnarfHandler methodsFor: 'smalltalk: debugging'!
inspect
	^InspectorView open: (SnarfHandlerInspector inspect: self)!
*/
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:51928:SnarfHandler methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static void linkTimeNonInherited() {
	Someone.hack();
	/* These don't use the full 32 bits so that we don't start manipulating LargeIntegers. */
	Flag = 1 << 25;
	Value = (1 << 25) - 1
	/* Flag - 1 */
	;
	SizeOffset = 4
	/* The offset of the size from the begginging of a mapCell */
	;
	UseFences = false;
/*
udanax-top.st:51940:SnarfHandler class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	
	self hack.  "These don't use the full 32 bits so that we don't start manipulating LargeIntegers."
	Flag _ 1 bitShift: 25.
	Value _ (1 bitShift: 25) - 1"Flag - 1".
	SizeOffset _ 4   "The offset of the size from the begginging of a mapCell".
	UseFences _ false!
*/
}
public static SnarfHandler make(SnarfHandle snarfHandle) {
	return new SnarfHandler(snarfHandle);
/*
udanax-top.st:51950:SnarfHandler class methodsFor: 'pcreate'!
make: snarfHandle {SnarfHandle}
	^self create: snarfHandle!
*/
}
/**
 * The number of bytes for one fence (Each flock requires two).
 */
public static int fenceSize() {
	if (UseFences) {
		return 4;
	}
	else {
		return 0;
	}
/*
udanax-top.st:51955:SnarfHandler class methodsFor: 'accessing'!
{Int32} fenceSize
	"The number of bytes for one fence (Each flock requires two)."
	
	UseFences ifTrue: [^4] ifFalse: [^Int32Zero]!
*/
}
/**
 * Return the number of bytes for a single map record, plus the space for the
 * fence. The fence will be just the index of the flock stored at the beginning and
 * the end of the flock's memory
 */
public static int mapCellOverhead() {
	return mapCellSize() + SnarfHandler.fenceSize() + SnarfHandler.fenceSize();
/*
udanax-top.st:51960:SnarfHandler class methodsFor: 'accessing'!
{Int32 INLINE} mapCellOverhead
	"Return the number of bytes for a single map record, plus the space for the 
	fence. The fence will be just the index of the flock stored at the beginning and 
	the end of the flock's memory"
	^self mapCellSize + SnarfHandler fenceSize + SnarfHandler fenceSize!
*/
}
/**
 * Return the number of bytes for a single map record.
 */
public static int mapCellSize() {
	return 8;
/*
udanax-top.st:51967:SnarfHandler class methodsFor: 'accessing'!
{Int32 INLINE} mapCellSize
	"Return the number of bytes for a single map record."
	
	^8!
*/
}
/**
 * The map starts just after the basic header.  The basic header currently has
 * the number of entries in the map and total amount of free space remaining.
 */
public static int mapOverhead() {
	return 8;
/*
udanax-top.st:51972:SnarfHandler class methodsFor: 'accessing'!
{Int32 INLINE} mapOverhead
	"The map starts just after the basic header.  The basic header currently has
	 the number of entries in the map and total amount of free space remaining."
	
	^8!
*/
}
/**
 * self sortTest: #(2 3 4 1).
 * self sortTest: #().
 * self sortTest: #(1000 1000 1000).
 * self sortTest: #(1 2 3 4).
 * self sortTest: #(1).
 * self sortTest: #(2 2 3 3 4 4 1 1).
 */
public static Pair sortTest(IntArray array) {
	Int32Array offsets;
	Int32Array indices;
	offsets = Int32Array.make(array.size() + 1);
	for (int i = 1; i <= array.size(); i ++ ) {
		offsets.store(i - 1, (array.at(i)));
	}
	offsets.store(array.size(), 1000);
	indices = sort(offsets, (IntegerSpace.make().getAscending()));
	return Pair.make(offsets, indices);
/*
udanax-top.st:51980:SnarfHandler class methodsFor: 'smalltalk: testing'!
{Pair of: UInt32Array with: UInt32Array} sortTest: array {Array of: IntegerVar}
	"self sortTest: #(2 3 4 1).
	self sortTest: #().
	self sortTest: #(1000 1000 1000).
	self sortTest: #(1 2 3 4).
	self sortTest: #(1).
	self sortTest: #(2 2 3 3 4 4 1 1)."
	
	| offsets {UInt32Array} indices {UInt32Array} |
	offsets _ UInt32Array make: array size + 1.
	1 to: array size do: [:i {IntegerVar} |
		offsets at: i - 1 store: (array at: i)].
	offsets at: array size store: 1000.
	indices _ self sort: offsets with: (IntegerSpace make getAscending).
	^Pair make: offsets with: indices.!
*/
}
/**
 * self sortTestDown: #(2 3 4 1).
 * self sortTestDown: #().
 * self sortTestDown: #(1000 1000 1000).
 * self sortTestDown: #(1 2 3 4).
 * self sortTestDown: #(1).
 * self sortTestDown: #(2 2 3 3 4 4 1 1).
 */
public static Pair sortTestDown(IntArray array) {
	Int32Array offsets;
	Int32Array indices;
	offsets = Int32Array.make(array.size() + 1);
	for (int i = 1; i <= array.size(); i ++ ) {
		offsets.store(i - 1, (array.at(i)));
	}
	offsets.store(array.size(), 1);
	indices = sort(offsets, (IntegerSpace.make().getDescending()));
	return Pair.make(offsets, indices);
/*
udanax-top.st:51996:SnarfHandler class methodsFor: 'smalltalk: testing'!
{Pair of: UInt32Array with: UInt32Array} sortTestDown: array {Array of: IntegerVar}
	"self sortTestDown: #(2 3 4 1).
	self sortTestDown: #().
	self sortTestDown: #(1000 1000 1000).
	self sortTestDown: #(1 2 3 4).
	self sortTestDown: #(1).
	self sortTestDown: #(2 2 3 3 4 4 1 1)."
	
	| offsets {UInt32Array} indices {UInt32Array} |
	offsets _ UInt32Array make: array size + 1.
	1 to: array size do: [:i {IntegerVar} |
		offsets at: i - 1 store: (array at: i)].
	offsets at: array size store: 1.
	indices _ self sort: offsets with: (IntegerSpace make getDescending).
	^Pair make: offsets with: indices.!
*/
}
public static void quickSort(Int32Array offsets, Int32Array indices, int first, int last) {
	int part;
	int left;
	int right;
	if (first >= last) {
		return ;
	}
	left = first;
	right = last + 1;
	swap(offsets, first, (left + right) / 2);
	swap(indices, first, (left + right) / 2);
	part = offsets.uIntAt(first);
	while (left < right) {
		left = left + 1;
		while ((offsets.uIntAt(left)) > part) {
			left = left + 1;
		}
		right = right - 1;
		while (part > (offsets.uIntAt(right))) {
			right = right-1;
		}
		if (left < right) {
			swap(offsets, left, right);
			swap(indices, left, right);
		}
	}
	swap(offsets, first, right);
	swap(indices, first, right);
	quickSort(offsets, indices, first, right - 1);
	quickSort(offsets, indices, right + 1, last);
/*
udanax-top.st:52014:SnarfHandler class methodsFor: 'private: sorting'!
{void} quickSort: offsets {UInt32Array} 
	with: indices {UInt32Array}
	with: first {Int32} 
	with: last {Int32}
	
	| part {Int32} left {Int32} right {Int32} |
	first >= last ifTrue: [^VOID].
	left _ first.
	right _ last + 1.
	self swap: offsets with: first with: (left + right) // 2.
	self swap: indices with: first with: (left + right) // 2.
	part _ offsets uIntAt: first.
	[left < right] whileTrue:
		[left _ left + 1.
		[(offsets uIntAt: left) > part] 
			whileTrue: [left _ left + 1].
		right _ right - 1.
		[part > (offsets uIntAt: right)]
			whileTrue: [right _ right -1].
		left < right ifTrue: 
			[self swap: offsets with: left with: right.
			self swap: indices with: left with: right]].
	self swap: offsets with: first with: right.
	self swap: indices with: first with: right.
	self quickSort: offsets with: indices with: first with: right - 1.
	self quickSort: offsets with: indices with: right + 1 with: last!
*/
}
public static void quickSort(Int32Array offsets, Int32Array indices, OrderSpec os, int first, int last) {
	int part;
	int left;
	int right;
	if (first >= last) {
		return ;
	}
	left = first;
	right = last + 1;
	swap(offsets, first, (left + right) / 2);
	swap(indices, first, (left + right) / 2);
	part = offsets.uIntAt(first);
	while (left < right) {
		left = left + 1;
		while ( ! (os.followsInt((offsets.uIntAt(left)), part))) {
			left = left + 1;
		}
		right = right - 1;
		while ( ! ((os.followsInt(part, (offsets.uIntAt(right)))))) {
			right = right-1;
		}
		if (left < right) {
			swap(offsets, left, right);
			swap(indices, left, right);
		}
	}
	swap(offsets, first, right);
	swap(indices, first, right);
	quickSort(offsets, indices, os, first, right - 1);
	quickSort(offsets, indices, os, right + 1, last);
/*
udanax-top.st:52041:SnarfHandler class methodsFor: 'private: sorting'!
{void} quickSort: offsets {UInt32Array} 
	with: indices {UInt32Array}
	with: os {OrderSpec} 
	with: first {IntegerVar} 
	with: last {IntegerVar}
	
	| part {IntegerVar} left {IntegerVar} right {IntegerVar} |
	first >= last ifTrue: [^VOID].
	left _ first.
	right _ last + 1.
	self swap: offsets with: first with: (left + right) // 2.
	self swap: indices with: first with: (left + right) // 2.
	part _ offsets uIntAt: first DOTasLong.
	[left < right] whileTrue:
		[left _ left + 1.
		[os followsInt: (offsets uIntAt: left DOTasLong) with: part] 
			whileFalse: [left _ left + 1].
		right _ right - 1.
		[(os followsInt: part with: (offsets uIntAt: right DOTasLong))]
			whileFalse: [right _ right -1].
		left < right ifTrue: 
			[self swap: offsets with: left with: right.
			self swap: indices with: left with: right]].
	self swap: offsets with: first with: right.
	self swap: indices with: first with: right.
	self quickSort: offsets with: indices with: os with: first with: right - 1.
	self quickSort: offsets with: indices with: os with: right + 1 with: last!
*/
}
/**
 * Sort the offsets array in place, and return an array of the same size that maps from the
 * new index of each element to its original index.  The offsets array is *assumed* to be
 * terminated with a guard element which is greater than or equal to all the other elements
 * of the array according to descending order.  If this isn't true, havoc may result.
 */
public static Int32Array sort(Int32Array offsets) {
	Int32Array result;
	result = Int32Array.make(offsets.count());
	for (int i = 0; i < offsets.count(); i ++ ) {
		result.storeUInt(i, i);
	}
	quickSort(offsets, result, 0, offsets.count() - 2);
	return result;
/*
udanax-top.st:52069:SnarfHandler class methodsFor: 'private: sorting'!
{UInt32Array} sort: offsets {UInt32Array}
	"Sort the offsets array in place, and return an array of the same size that maps from the new index of each element to its original index.  The offsets array is *assumed* to be terminated with a guard element which is greater than or equal to all the other elements of the array according to descending order.  If this isn't true, havoc may result."
	
	| result {UInt32Array} |
	result _ UInt32Array make: offsets count.
	Int32Zero almostTo: offsets count do: [:i {Int32} |
		result at: i storeUInt: i].
	self
		quickSort: offsets
		with: result
		with: Int32Zero
		with: offsets count - 2.
	^result!
*/
}
/**
 * Sort the offsets array in place, and return an array of the same size that maps from the
 * new index of each element to its original index.  The offsets array is *assumed* to be
 * terminated with a guard element which is greater than or equal to all the other elements
 * of the array according to the sorting order.  If this isn't true, havoc may result.
 */
public static Int32Array sort(Int32Array offsets, OrderSpec os) {
	Int32Array result;
	result = Int32Array.make(offsets.count());
	for (int i = 0; i < offsets.count(); i ++ ) {
		result.storeUInt(i, i);
	}
	quickSort(offsets, result, os, 0, offsets.count() - 2);
	return result;
/*
udanax-top.st:52083:SnarfHandler class methodsFor: 'private: sorting'!
{UInt32Array} sort: offsets {UInt32Array} with: os {OrderSpec}
	"Sort the offsets array in place, and return an array of the same size that maps from the new index of each element to its original index.  The offsets array is *assumed* to be terminated with a guard element which is greater than or equal to all the other elements of the array according to the sorting order.  If this isn't true, havoc may result."
	
	| result {UInt32Array} |
	result _ UInt32Array make: offsets count.
	Int32Zero almostTo: offsets count do: [:i {Int32} |
		result at: i storeUInt: i].
	self
		quickSort: offsets
		with: result
		with: os
		with: Int32Zero
		with: offsets count - 2.
	^result!
*/
}
public static void swap(Int32Array array, int i, int j) {
	int temp;
	temp = array.uIntAt(i);
	array.storeUInt(i, (array.uIntAt(j)));
	array.storeUInt(j, temp);
/*
udanax-top.st:52098:SnarfHandler class methodsFor: 'private: sorting'!
{void INLINE} swap: array {UInt32Array} 
	with: i {IntegerVar} 
	with: j {IntegerVar}
	
	| temp {UInt32} |
	temp _ array uIntAt: i DOTasLong.
	array at: i DOTasLong storeUInt: (array uIntAt: j DOTasLong).
	array at: j DOTasLong storeUInt: temp!
*/
}
public SnarfHandler() {
/*

Generated during transformation
*/
}
public SnarfHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
