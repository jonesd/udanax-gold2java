/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.primtab.PrimPtrTableStepper;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.FlockLocation;
import info.dgjones.abora.gold.snarf.Pumpkin;
import info.dgjones.abora.gold.snarf.SnarfHandler;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.snarf.SnarfRecord;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Manage retrieval, refitting, and rewriting of existing flocks.  Assign indices for new
 * flocks.
 * SnarfRecords can go away after their contents have been flushed.  We might keep it around
 * if we expect to be assigning new flocks to the snarf again, just to keep myOccupied.  The
 * snarfRecord will be recreated when another object is read in.
 */
public class SnarfRecord extends Heaper {

	protected int mySnarfID;
	protected SnarfPacker myPacker;
	protected int mySpaceLeft;
	protected IntegerRegion myOccupied;
	protected PrimPtrTable myChangedFlocks;
	protected int myDestroyCount;
/*
udanax-top.st:52314:
Heaper subclass: #SnarfRecord
	instanceVariableNames: '
		mySnarfID {SnarfID}
		myPacker {SnarfPacker}
		mySpaceLeft {Int32}
		myOccupied {IntegerRegion | NULL}
		myChangedFlocks {PrimPtrTable of: Abraham}
		myDestroyCount {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:52324:
SnarfRecord comment:
'Manage retrieval, refitting, and rewriting of existing flocks.  Assign indices for new flocks.  
SnarfRecords can go away after their contents have been flushed.  We might keep it around if we expect to be assigning new flocks to the snarf again, just to keep myOccupied.  The snarfRecord will be recreated when another object is read in.'!
*/
/*
udanax-top.st:52328:
(SnarfRecord getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:52620:
SnarfRecord class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:52623:
(SnarfRecord getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SnarfRecord.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Shep is being newly added to this snarf.  Allocate enough space for it and return the
 * newly assigned index for it.
 */
public int allocate(int size, Abraham shep) {
	/* The spaceLeft that we compute includes the size of the cells, otherwise we couldn't keep the number up to date. */
	int index;
	if ( ! (size <= mySpaceLeft)) {
		throw new AboraAssertionException("Must have space left");
	}
	if ( ! ( ! (shep.isEqual(Pumpkin.make())))) {
		throw new AboraAssertionException("Only allocate real shepherds");
	}
	if ( ! ( ! shep.isStub())) {
		throw new AboraAssertionException("Must be instantiated");
	}
	Someone.thingToDo();
	/* A hash check to see if shep is being forwarded back to this snarf from elsewhere. */
	index = allocateIndex();
	shep.getInfo().setSize(size - SnarfHandler.mapCellOverhead());
	setSpaceLeft(mySpaceLeft - size);
	myChangedFlocks.store(index, shep);
	return index;
/*
udanax-top.st:52333:SnarfRecord methodsFor: 'writing'!
{Int32} allocate: size {Int32} with: shep {Abraham}
	"Shep is being newly added to this snarf.  Allocate enough space for it and return the newly assigned index for it."
	"The spaceLeft that we compute includes the size of the cells, otherwise we couldn't keep the number up to date."
	
	| index {IntegerVar} |
	size <= mySpaceLeft assert: 'Must have space left'.
	(shep isEqual: Pumpkin make) not assert: 'Only allocate real shepherds'. 
	shep isStub not assert: 'Must be instantiated'.
	self thingToDo.  "A hash check to see if shep is being forwarded back to this snarf from elsewhere."
	index _ self allocateIndex.
	shep getInfo setSize: size - SnarfHandler mapCellOverhead.
	self setSpaceLeft: mySpaceLeft - size.
	myChangedFlocks at: index store: shep.
	^index DOTasLong!
*/
}
/**
 * Remember that the flock at index must be written to the snarf on the next update.
 */
public void changedFlock(int index, Abraham shep) {
	if ( ! ( ! (shep.isEqual(Pumpkin.make())))) {
		throw new AboraAssertionException("Record changes for real objects only");
	}
	if ( ! ( ! 
	/* We don't return the flock's space to the pool here because it might be a forwarded flock. */
	(Heaper.isDestructed(shep)))) {
		throw new AboraAssertionException("Must not be destructed");
	}
	if ( ! ( ! shep.isStub())) {
		throw new AboraAssertionException("Must be instantiated");
	}
	myChangedFlocks.store(index, shep);
/*
udanax-top.st:52348:SnarfRecord methodsFor: 'writing'!
{void} changedFlock: index {Int32} with: shep {Abraham}
	"Remember that the flock at index must be written to the snarf on the next update."
	
	(shep isEqual: Pumpkin make) not assert: 'Record changes for real objects only'.
	"We don't return the flock's space to the pool here because it might be a forwarded flock."
	(Heaper isDestructed: shep) not assert: 'Must not be destructed'.
	shep isStub not assert: 'Must be instantiated'.
	myChangedFlocks at: index store: shep!
*/
}
/**
 * Remove the flock from the disk.  Replace it with a Pumpkin so that the
 * routine that flushes to disk knows to remove whatever's there already.
 */
public void dismantleFlock(FlockInfo info) {
	/* Remove the flocks space allocation now so that we can reallocate from the newly created pool. */
	Abraham tmp;
	tmp = (Abraham) myChangedFlocks.fetch(info.index());
	if (tmp != null && (info.fetchShepherd() != tmp)) {
		halt();
	}
	setSpaceLeft(mySpaceLeft + info.oldSize());
	myChangedFlocks.store(info.index(), Pumpkin.make());
	myDestroyCount = myDestroyCount + 1;
/*
udanax-top.st:52357:SnarfRecord methodsFor: 'writing'!
{void} dismantleFlock: info {FlockInfo}
	"Remove the flock from the disk.  Replace it with a Pumpkin so that the 
	 routine that flushes to disk knows to remove whatever's there already."
	"Remove the flocks space allocation now so that we can reallocate from the newly created pool."
	[|tmp {Abraham}|
	tmp _ myChangedFlocks fetch: info index.
	(tmp ~~ NULL and: [info fetchShepherd ~~ tmp]) ifTrue: [self halt]] smalltalkOnly.
	self setSpaceLeft: mySpaceLeft + info oldSize.
	myChangedFlocks at: info index store: Pumpkin make.
	myDestroyCount _ myDestroyCount + 1!
*/
}
/**
 * Rewrite all flocks that have changed in this snarf.
 */
public void flushChanges() {
	int highest;
	SnarfHandler handler;
	int newHighest;
	PrimPtrTableStepper stepper;
	Abraham shep;
	handler = getWriteHandler();
	highest = handler.mapCount();
	newHighest = wipeBelowHighest(highest, handler);
	/* mySpaceLeft also has the size of the cells taken out of it. */
	Someone.thingToDo();
	/* Depending on tests, this might also preclear the total space for all
		of the flocks to be written.  Then we will only compact once, and do
		it before writing any flocks. */
	Someone.hack();
	/* This should get the highest index from myOccupied, except that it might not be computed. */
	handler.allocateCells(newHighest - highest);
	stepper = myChangedFlocks.stepper();
	while ((shep = (Abraham) stepper.fetch()) != null) {
		int index;
		index = stepper.index();
		if ( ! (shep.isEqual(Pumpkin.make()))) {
			if (shep.getInfo().snarfID() == mySnarfID) {
				/* Not forwarded. */
				Xmtr xmtr;
				XnWriteStream stream;
				if ( ! ( ! shep.isStub())) {
					throw new AboraAssertionException("Must be instantiated");
				}
				handler.allocate(index, shep.getInfo().oldSize());
				stream = handler.writeStream(index);
				xmtr = myPacker.makeXmtr(stream);
				xmtr.sendHeaper(shep);
				xmtr.destroy();
				stream.destroy();
				handler.storeForget(index, shep.getInfo().isForgotten());
				shep.getInfo().commitFlags();
				shep.getInfo().clearContentsDirty();
			}
			else {
				/* We only get here for forwarded flocks. */
				handler.forwardTo(index, shep.getInfo().snarfID(), shep.getInfo().index());
			}
		}
		stepper.step();
	}
	stepper.destroy();
	myChangedFlocks.clearAll();
	handler.destroy();
/*
udanax-top.st:52371:SnarfRecord methodsFor: 'transactions'!
{void} flushChanges
	"Rewrite all flocks that have changed in this snarf."
	| highest {Int32} handler {SnarfHandler} newHighest {IntegerVar} stepper {PrimPtrTableStepper} shep {Abraham} |
	handler _ self getWriteHandler.
	highest _ handler mapCount.
	newHighest _ self wipeBelowHighest: highest with: handler.
	"mySpaceLeft also has the size of the cells taken out of it."
	self thingToDo. 
		"Depending on tests, this might also preclear the total space for all
		of the flocks to be written.  Then we will only compact once, and do
		it before writing any flocks."
	self hack.  "This should get the highest index from myOccupied, except that it might not be computed."
	handler allocateCells: newHighest - highest.
	stepper := myChangedFlocks stepper.
	[(shep := stepper fetch cast: Abraham) ~~ NULL] whileTrue: 
		[| index {IntegerVar} |
		index := stepper index.
		(shep isEqual: Pumpkin make) ifFalse:
			[shep getInfo snarfID == mySnarfID	 
				ifTrue:
					["Not forwarded."
					| xmtr {Xmtr} stream {XnWriteStream} |
					shep isStub not assert: 'Must be instantiated'.
					handler at: index allocate: shep getInfo oldSize.
					stream _ handler writeStream: index.
					xmtr _ myPacker makeXmtr: stream.
					xmtr sendHeaper: shep.
					xmtr destroy.
					stream destroy.
					handler at: index DOTasLong storeForget: shep getInfo isForgotten.
					shep getInfo commitFlags.
					shep getInfo clearContentsDirty]
				ifFalse:
					["We only get here for forwarded flocks."
					handler forward: index to: shep getInfo snarfID with: shep getInfo index]].
		stepper step].
	stepper destroy.
	myChangedFlocks clearAll.
	handler destroy!
*/
}
/**
 * Recompute size information for all changed shepherds and see if they still fit.
 * Any that don't get handed to the SnarfPacker to treat as new flocks.   The
 * old space changed and dismantled flocks has been returned to the pool.
 * Reallocate space for the changed flocks out of the pool.  Any that don't fit
 * are handed back to myPacker to go in other snarfs.
 */
public void refitFlocks() {
	Stepper stomper = myChangedFlocks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Abraham shep = (Abraham) stomper.fetch();
		if (shep == null) {
			continue ;
		}
		setSpaceLeft(mySpaceLeft + shep.getInfo().oldSize());
		if ( ! (mySpaceLeft >= 0)) {
			throw new AboraAssertionException("Must have space left");
		}
	}
	stomper.destroy();
	Stepper stomper2 = myChangedFlocks.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		Abraham shep1 = (Abraham) stomper2.fetch();
		if (shep1 == null) {
			continue ;
		}
		/* Leave Pumpkins here so they will be seen by flushChanges. */
		if ( ! (shep1.isEqual(Pumpkin.make()))) {
			int size;
			size = myPacker.computeSize(shep1);
			shep1.getInfo().setSize(size);
			if (size <= mySpaceLeft) {
				setSpaceLeft(mySpaceLeft - size);
			}
			else {
				myPacker.forwardFlock(shep1);
			}
		}
		if ( ! (mySpaceLeft >= 0)) {
			throw new AboraAssertionException("Must have space left");
		}
	}
	stomper2.destroy();
/*
udanax-top.st:52412:SnarfRecord methodsFor: 'transactions'!
{void} refitFlocks
	"Recompute size information for all changed shepherds and see if they still fit.
	 Any that don't get handed to the SnarfPacker to treat as new flocks.   The 
	 old space changed and dismantled flocks has been returned to the pool.  
	 Reallocate space for the changed flocks out of the pool.  Any that don't fit 
	 are handed back to myPacker to go in other snarfs."
	
	myChangedFlocks stepper forEach: 
		[:shep {Abraham} |
		self setSpaceLeft: mySpaceLeft + shep getInfo oldSize.
 		mySpaceLeft >= Int32Zero assert: 'Must have space left'].
	myChangedFlocks stepper forEach: 
		[:shep {Abraham} |
		"Leave Pumpkins here so they will be seen by flushChanges."
		(shep isEqual: Pumpkin make) not ifTrue: 
			[|  size {Int32} |
			size _ myPacker computeSize: shep.
			shep getInfo setSize: size.
			size <= mySpaceLeft 
				ifTrue: [self setSpaceLeft: mySpaceLeft - size]
				ifFalse: [myPacker forwardFlock: shep]].
 		mySpaceLeft >= Int32Zero assert: 'Must have space left'.]!
*/
}
/**
 * Return the amount of space currently left in the snarf.
 */
public int spaceLeft() {
	return mySpaceLeft;
/*
udanax-top.st:52435:SnarfRecord methodsFor: 'transactions'!
{Int32} spaceLeft
	"Return the amount of space currently left in the snarf."
	
	^mySpaceLeft!
*/
}
/**
 * Destroy all objects imaged from this snarf.
 */
public void destruct() {
	myChangedFlocks.destroy();
	if (myOccupied != null) {
		myOccupied.destroy();
	}
	super.destruct();
/*
udanax-top.st:52442:SnarfRecord methodsFor: 'protected: destruct'!
{void} destruct
	"Destroy all objects imaged from this snarf."
	
	myChangedFlocks destroy.
	myOccupied ~~ NULL ifTrue: [myOccupied destroy].
	super destruct.!
*/
}
/**
 * Return the first unoccupied index in the snarf.  Compute the lowest
 * element >= 0 that is not already in the occupied region by subtracting
 * the occupied region from the region >= 0.
 */
public int allocateIndex() {
	int index;
	readOccupied();
	index = myOccupied.nearestIntHole(0);
	myOccupied = (IntegerRegion) (myOccupied.withInt(index));
	return index;
/*
udanax-top.st:52451:SnarfRecord methodsFor: 'private: private'!
{IntegerVar} allocateIndex
	"Return the first unoccupied index in the snarf.  Compute the lowest
	 element >= 0 that is not already in the occupied region by subtracting 
	 the occupied region from the region >= 0."
	
	| index {IntegerVar} |
	self readOccupied.
	index _ myOccupied nearestIntHole: IntegerVar0.
	myOccupied _ (myOccupied withInt: index) cast: IntegerRegion.
	^index!
*/
}
/**
 * Get the handler for my snarf so that I can send or receive data from it.
 */
public SnarfHandler getWriteHandler() {
	SnarfHandler handler;
	boolean flag;
	flag = myOccupied != null && (myOccupied.count() == myChangedFlocks.count());
	/* We also need to compare regions in case as many things are dismantled as are unchanged. */
	/* Change this to iterate myOCcupied and check the presence of each element. 
	 Either that or use an IntegerTable for myChangedFlocks. */
	/* myChangedFlocks really wants to be an optimizing representation. */
	if (flag) {
		PrimPtrTableStepper stepper;
		/* calculate myOccupied isSuperSetOf: myChangedFlocks domain */
		stepper = myChangedFlocks.stepper();
		while (flag && (stepper.hasValue())) {
			if ( ! (myOccupied.hasIntMember(stepper.index()))) {
				flag = false;
			}
			stepper.step();
		}
		flag = flag && ( ! stepper.hasValue());
		stepper.destroy();
	}
	if (flag) {
		handler = SnarfHandler.make((myPacker.currentView().makeErasingHandle(mySnarfID)));
		handler.initializeSnarf();
	}
	else {
		handler = SnarfHandler.make((myPacker.currentView().makeReadHandle(mySnarfID)));
	}
	handler.makeWritable();
	return handler;
/*
udanax-top.st:52462:SnarfRecord methodsFor: 'private: private'!
{SnarfHandler} getWriteHandler
	"Get the handler for my snarf so that I can send or receive data from it."
	| handler {SnarfHandler} flag {BooleanVar} |
	flag := myOccupied ~~ NULL and: [myOccupied count == myChangedFlocks count].
	"We also need to compare regions in case as many things are dismantled as are unchanged."
	"Change this to iterate myOCcupied and check the presence of each element. 
	 Either that or use an IntegerTable for myChangedFlocks."
	"myChangedFlocks really wants to be an optimizing representation."
	flag ifTrue: [
		| stepper {PrimPtrTableStepper} |
		"calculate myOccupied isSuperSetOf: myChangedFlocks domain"
		stepper := myChangedFlocks stepper.
		[flag and: [stepper hasValue]] whileTrue: [
			(myOccupied hasIntMember: stepper index) ifFalse: [flag := false].
			stepper step].
		flag := flag and: [stepper hasValue not].
		stepper destroy].
	
	flag	
		ifTrue:
			[handler _ SnarfHandler make: (myPacker currentView makeErasingHandle: mySnarfID).
			handler initializeSnarf]
		ifFalse:
			[handler _ SnarfHandler make: (myPacker currentView makeReadHandle: mySnarfID)].
	handler makeWritable.
	^handler!
*/
}
/**
 * Create an array with the sizes of every flock in the snarf.
 */
public void readOccupied() {
	SnarfHandler handler;
	int count;
	if (myOccupied != null) {
		return ;
	}
	if (mySpaceLeft >= (myPacker.currentView().getDataSizeOfSnarf(mySnarfID))) {
		myOccupied = IntegerRegion.make();
		return ;
	}
	handler = SnarfHandler.make((myPacker.currentView().makeReadHandle(mySnarfID)));
	count = handler.mapCount();
	myOccupied = IntegerRegion.make(0, count);
	for (int i = 0; i < count; i ++ ) {
		Abraham shep;
		shep = (Abraham) (myChangedFlocks.fetch(i));
		if ( ! (handler.isOccupied(i)) || (shep != null && (shep.isEqual(Pumpkin.make())))) {
			myOccupied = (IntegerRegion) (myOccupied.without(IntegerPos.make(i)));
		}
	}
	handler.destroy();
/*
udanax-top.st:52490:SnarfRecord methodsFor: 'private: private'!
{void} readOccupied
	"Create an array with the sizes of every flock in the snarf."
	
	| handler {SnarfHandler} count {Int32} |
	myOccupied ~~ NULL ifTrue: [^VOID].
	mySpaceLeft >= (myPacker currentView getDataSizeOfSnarf: mySnarfID) ifTrue:
		[myOccupied _ IntegerRegion make.
		^VOID].
	handler _ SnarfHandler make: (myPacker currentView makeReadHandle: mySnarfID).
	count _ handler mapCount.
	myOccupied _ IntegerRegion make: IntegerVar0 with: count.
	Int32Zero almostTo: count do:
		[:i {Int32} |
		| shep {Abraham | NULL} |
		shep _ (myChangedFlocks fetch: i) cast: Abraham.
		((handler isOccupied: i) not or: [shep ~~ NULL and: [shep isEqual: Pumpkin make]])
			ifTrue: [myOccupied _ (myOccupied without: i integer) cast: IntegerRegion]].
	handler destroy!
*/
}
public void setSpaceLeft(int spaceLeft) {
	if ( ! (spaceLeft >= 0)) {
		throw new AboraAssertionException("Space is positive");
	}
	mySpaceLeft = spaceLeft;
/*
udanax-top.st:52509:SnarfRecord methodsFor: 'private: private'!
{void} setSpaceLeft: spaceLeft {Int32}
	spaceLeft >= Int32Zero assert: 'Space is positive'.
	mySpaceLeft _ spaceLeft.!
*/
}
public int wipeBelowHighest(int highest, SnarfHandler handler) {
	int newHighest;
	PrimPtrTableStepper stepper;
	/* (myChangedFlocks domain intersect: (IntegerRegion before: highest)) stepper forEach: 
		[:key {XnInteger} | handler wipeFlock: key asIntegerVar]. ----  too inefficient.  also compute the upper bound for later. */
	newHighest = highest;
	stepper = myChangedFlocks.stepper();
	while (stepper.hasValue()) {
		int index;
		index = stepper.index();
		if (index < highest) {
			handler.wipeFlock(index);
		}
		if (index >= newHighest) {
			newHighest = index + 1
			/* Must be above the new key. */
			;
		}
		stepper.step();
	}
	stepper.destroy();
	return newHighest;
/*
udanax-top.st:52513:SnarfRecord methodsFor: 'private: private'!
{IntegerVar} wipeBelowHighest: highest {Int32} with: handler {SnarfHandler}
	| newHighest {IntegerVar} stepper {PrimPtrTableStepper} |
	"(myChangedFlocks domain intersect: (IntegerRegion before: highest)) stepper forEach: 
		[:key {XnInteger} | handler wipeFlock: key asIntegerVar]. ----  too inefficient.  also compute the upper bound for later." 
	newHighest _ highest.
	stepper _ myChangedFlocks stepper.
	[stepper hasValue] whileTrue: 
		[| index {IntegerVar} |
		index := stepper index.
		index < highest ifTrue: [handler wipeFlock: index].
		index >= newHighest ifTrue: [newHighest _ index+1   "Must be above the new key."].
		stepper step].
	stepper destroy.
	^ newHighest!
*/
}
public SnarfRecord(int snarfID, SnarfPacker packer, int spaceLeft) {
	super();
	mySnarfID = snarfID;
	myPacker = packer;
	myChangedFlocks = PrimPtrTable.make(128);
	setSpaceLeft(spaceLeft);
	myOccupied = null;
	if (mySpaceLeft >= (myPacker.currentView().getDataSizeOfSnarf(mySnarfID))) {
		mySpaceLeft = (myPacker.currentView().getDataSizeOfSnarf(mySnarfID)) - SnarfHandler.mapOverhead();
		myOccupied = IntegerRegion.make();
	}
	myDestroyCount = 0;
/*
udanax-top.st:52530:SnarfRecord methodsFor: 'create'!
create: snarfID {SnarfID} with: packer {SnarfPacker} with: spaceLeft {Int32}
	super create.
	mySnarfID _ snarfID.
	myPacker _ packer.
	myChangedFlocks _ PrimPtrTable make: 128.
	self setSpaceLeft: spaceLeft.
	myOccupied _ NULL.
	mySpaceLeft >= (myPacker currentView getDataSizeOfSnarf: mySnarfID) ifTrue: 
		[mySpaceLeft _ (myPacker currentView getDataSizeOfSnarf: mySnarfID) - SnarfHandler mapOverhead.
		myOccupied _ IntegerRegion make].
	myDestroyCount _ Int32Zero!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(mySnarfID);
	oo.print(")");
/*
udanax-top.st:52544:SnarfRecord methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << mySnarfID << ')'!
*/
}
/**
 * If the location specified by info has been forwarded, return a flockInfo
 * describing its new location.
 * @deprecated
 */
public FlockLocation fetchForward(int index) {
	throw new PasseException();
/*
udanax-top.st:52549:SnarfRecord methodsFor: 'smalltalk: passe'!
{FlockLocation} fetchForward: index {Int32}
	"If the location specified by info has been forwarded, return a flockInfo
	 describing its new location."
	
	self passe!
*/
}
/**
 * Return the set of indices to locations that are forgotten.
 * @deprecated
 */
public MuSet forgottenFlocks() {
	throw new PasseException();
/*
udanax-top.st:52555:SnarfRecord methodsFor: 'smalltalk: passe'!
{MuSet of: IntegerPos} forgottenFlocks
	"Return the set of indices to locations that are forgotten."
	
	self passe!
*/
}
/**
 * Return true if the flock at that location is forgotten. Higher level routines
 * should make sure this doesn't get done very often because it requires bringing
 * in the snarf if it's not already there.
 * @deprecated
 */
public boolean isForgotten(int index) {
	throw new PasseException();
/*
udanax-top.st:52560:SnarfRecord methodsFor: 'smalltalk: passe'!
{BooleanVar} isForgotten: index {Int32} 
	"Return true if the flock at that location is forgotten. Higher level routines 
	should make sure this doesn't get done very often because it requires bringing 
	in the snarf if it's not already there."
	self passe!
*/
}
/**
 * Return true if everything in this snarfRecord is purged.
 * If so, then this snarfRecord can be thrown away.
 * @deprecated
 */
public boolean isPurgeable() {
	throw new PasseException();
/*
udanax-top.st:52567:SnarfRecord methodsFor: 'smalltalk: passe'!
{BooleanVar} isPurgeable
	"Return true if everything in this snarfRecord is purged. 
	 If so, then this snarfRecord can be thrown away."
	
	self passe!
*/
}
/**
 * We know that the object wasn't imaged.  Read the real shepherd into
 * the memory occupied by stub.  If the location is a forwarder, then
 * register a new flockInfo with the stub and just return.
 * @deprecated
 */
public void makeReal(int index, Abraham stub) {
	throw new PasseException();
/*
udanax-top.st:52573:SnarfRecord methodsFor: 'smalltalk: passe'!
{void} makeReal: index {Int32} with: stub {Abraham}
	"We know that the object wasn't imaged.  Read the real shepherd into
	 the memory occupied by stub.  If the location is a forwarder, then 
	 register a new flockInfo with the stub and just return."
	
	| handler {SnarfHandler} loc {FlockLocation | NULL} |
	stub isStub assert: 'Only stubs can be made real'.
	handler _ self getReadHandler.
	self readOccupied.
	loc _ handler fetchForward: index.
	loc == NULL
		ifTrue: 
			[| info {FlockInfo} oldHash {UInt32} |
			info _ stub getInfo.
			oldHash _ stub hashForEqual.
			(myPacker makeRcvr: (handler readStream: index) with: mySnarfID with: index)
					receiveInto: stub.
			stub hashForEqual == oldHash assert: 'Hash must not change'.
			info setSize: (handler flockSize: index).
			stub flockInfo: info]
		ifFalse:
			["Forwarded.  Register stub at the new location." 
			stub flockInfo: (FlockInfo make: stub getInfo with: loc snarfID with: loc index).
			myPacker addInfo: stub getInfo with: stub].
	self releaseReadHandler!
*/
}
/**
 * This will get a flock that we know the location of
 * without a stub.  The flock must not already be imaged,
 * and it must not be forwarded.
 * @deprecated
 */
public Abraham originateFlock(int index) {
	throw new PasseException();
/*
udanax-top.st:52599:SnarfRecord methodsFor: 'smalltalk: passe'!
{Abraham} originateFlock: index {IntegerVar}
	"This will get a flock that we know the location of
	 without a stub.  The flock must not already be imaged,
	 and it must not be forwarded."
	
	| result {Abraham} rcvr {Rcvr} |
	rcvr _ myPacker makeRcvr: (self getReadHandler readStream: index DOTasLong)
					with: mySnarfID 
					with: index.
	result _ rcvr receiveHeaper cast: Abraham.
	self readOccupied.
	self releaseReadHandler. 
	^result!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:52615:SnarfRecord methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:52617:SnarfRecord methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static SnarfRecord make(int snarfID, SnarfPacker packer, int spaceLeft) {
	return new SnarfRecord(snarfID, packer, spaceLeft);
/*
udanax-top.st:52628:SnarfRecord class methodsFor: 'pcreate'!
make: snarfID {SnarfID} with: packer {SnarfPacker} with: spaceLeft {Int32}
	^self create: snarfID with: packer with: spaceLeft!
*/
}
public SnarfRecord() {
/*

Generated during transformation
*/
}
public SnarfRecord(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public SnarfHandler getReadHandler() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
public void releaseReadHandler() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
