/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.settable.SetTable;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.gchooks.SanitationEngineer;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.BlockClosure;
import info.dgjones.abora.gold.java.missing.smalltalk.Collection;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.urdi.Urdi;
import info.dgjones.abora.gold.java.urdi.UrdiView;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.packer.PersistentCleaner;
import info.dgjones.abora.gold.packer.SpareStageSpace;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.purging.LiberalPurgeror;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskCountSpecialist;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.FlockLocation;
import info.dgjones.abora.gold.snarf.Pumpkin;
import info.dgjones.abora.gold.snarf.Purgeror;
import info.dgjones.abora.gold.snarf.SnarfHandler;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.snarf.SnarfRecord;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.stacker.StackExaminer;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.urdi.CountStream;
import info.dgjones.abora.gold.urdi.SnarfInfoHandler;
import info.dgjones.abora.gold.xcvr.DiskSpecialist;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Should myFlocks contain full flockInfos for forwarded flocks?  Both the flags and the size
 * mean nothing.
 * A SnarfPacker maintains the relationship between Shepherds and the set of snarfs
 * representing the disk.  A SnarfPacker assigns flocks to snarfs based loosely on the
 * flocks''s Shepherd''s preferences.  When a flock changes, it informs the SnarfPacker.
 * When the SnarfPacker decides to write to the disk, it ensures that the changed objects
 * still fit in their snarf (migrating them if necessary), writes them to the snarf, then
 * writes out the snarf.
 * mySnarfInfo {MuTable of: XuInteger}
 * - How much space remains in each snarf.
 * mySnarfMap {MuTable of: SnarfRecord}
 * - Map from snarfIDs to a SnarfRecord that handles that snarf.
 * myChangedSnarfs {MuSet of: XuInteger}
 * - The IDs for all snarfs in which an imaged flock has changed.
 * myFlocks {SetTable of: XuInteger and: FlockInfo}
 * - Indexed by Abraham hash, contains all FlockInfos that refer to flocks in memory.
 * Multiple infos may refer to the same flock if it is referenced through forwarding.
 * The only info considered to have the correct state wrt its flocks suitability for
 * purging is the info pointed to by its Abraham.
 * myInsideCommit {BooleanVar}
 * - True while writing new and changed flocks to disk to prevent purging,
 * and during purgeClean to prevent recursive call through Purgeror recycling.
 */
public class SnarfPacker extends DiskManager {

	protected SnarfInfoHandler mySnarfInfo;
	protected Turtle myTurtle;
	protected int myAllocationSnarf;
	protected MuTable mySnarfMap;
	protected SetTable myFlocks;
	protected IntegerTable myNewFlocks;
	protected int myLastNewCount;
	protected int myNewEstimate;
	protected MuArray myDestroyedFlocks;
	protected UrdiView myUrdiView;
	protected Urdi myUrdi;
	protected XcvrMaker myXcvrMaker;
	protected Cookbook myBook;
	protected Counter myNextHash;
	protected int myConsistentCount;
	protected boolean myInsideCommit;
	protected int myDestroyCount;
	protected SanitationEngineer myPurgeror;
	protected LiberalPurgeror myRepairer;
	protected static Collection DebugSizes;
	protected static int LRUCount;
/*
udanax-top.st:16843:
DiskManager subclass: #SnarfPacker
	instanceVariableNames: '
		mySnarfInfo {SnarfInfoHandler}
		myTurtle {Turtle | NULL}
		myAllocationSnarf {SnarfID}
		mySnarfMap {MuTable of: IntegerPos with: SnarfRecord}
		myFlocks {SetTable of: IntegerPos and: FlockInfo}
		myNewFlocks {IntegerTable of: FlockInfo}
		myLastNewCount {IntegerVar}
		myNewEstimate {IntegerVar}
		myDestroyedFlocks {MuArray of: Abraham}
		myUrdiView {UrdiView star}
		myUrdi {Urdi star}
		myXcvrMaker {XcvrMaker}
		myBook {Cookbook}
		myNextHash {Counter}
		myConsistentCount {IntegerVar}
		myInsideCommit {BooleanVar}
		myDestroyCount {IntegerVar}
		myPurgeror {SanitationEngineer}
		myRepairer {LiberalPurgeror}'
	classVariableNames: '
		DebugSizes {Collection smalltalk} 
		LRUCount {Int32} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:16868:
SnarfPacker comment:
'Should myFlocks contain full flockInfos for forwarded flocks?  Both the flags and the size mean nothing.
A SnarfPacker maintains the relationship between Shepherds and the set of snarfs representing the disk.  A SnarfPacker assigns flocks to snarfs based loosely on the flocks''s Shepherd''s preferences.  When a flock changes, it informs the SnarfPacker.  When the SnarfPacker decides to write to the disk, it ensures that the changed objects still fit in their snarf (migrating them if necessary), writes them to the snarf, then writes out the snarf.
mySnarfInfo {MuTable of: XuInteger}
		- How much space remains in each snarf.
mySnarfMap {MuTable of: SnarfRecord}
		- Map from snarfIDs to a SnarfRecord that handles that snarf.
myChangedSnarfs {MuSet of: XuInteger}
		- The IDs for all snarfs in which an imaged flock has changed.
myFlocks {SetTable of: XuInteger and: FlockInfo}
		- Indexed by Abraham hash, contains all FlockInfos that refer to flocks in memory.
		  Multiple infos may refer to the same flock if it is referenced through forwarding.
		  The only info considered to have the correct state wrt its flocks suitability for
		  purging is the info pointed to by its Abraham.
myInsideCommit {BooleanVar}
		- True while writing new and changed flocks to disk to prevent purging,
		  and during purgeClean to prevent recursive call through Purgeror recycling.'!
*/
/*
udanax-top.st:16887:
(SnarfPacker getOrMakeCxxClassDescription)
	friends:
'friend class ResetCommit_Bomb;
friend class CBlockTrackingPacker;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:17610:
SnarfPacker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:17613:
(SnarfPacker getOrMakeCxxClassDescription)
	friends:
'friend class ResetCommit_Bomb;
friend class CBlockTrackingPacker;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SnarfPacker.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Queue destroy of the given flock.  The destroy will happen later.
 */
public void destroyFlock(FlockInfo info) {
	Abraham flock;
	flock = info.getShepherd();
	if (Heaper.isDestructed(flock)) {
		throw new AboraRuntimeException(AboraRuntimeException.DESTRUCTED_ABE);
	}
	info.markDestroyed();
	if (info.markForgotten()) {
		recordUpdate(info);
	}
	if (info.isNew()) {
		flock = flock
		/* just so I can set a breakpoint */
		;
	}
	else {
		mySnarfInfo.setForgottenFlag(info.snarfID(), true);
	}
	myDestroyedFlocks.intIntroduce(myDestroyedFlocks.count(), flock);
/*
udanax-top.st:16895:SnarfPacker methodsFor: 'shepherds'!
{void} destroyFlock: info {FlockInfo} 
	"Queue destroy of the given flock.  The destroy will happen later."
	| flock {Abraham} |
	flock _ info getShepherd.
	(Heaper isDestructed: flock) ifTrue: [Heaper BLAST: #DestructedAbe].
	info markDestroyed.
	info markForgotten ifTrue: [self recordUpdate: info].
	info isNew 
		ifTrue: [flock _ flock "just so I can set a breakpoint"]
		ifFalse: [mySnarfInfo setForgottenFlag: info snarfID with: true].
	myDestroyedFlocks atInt: myDestroyedFlocks count introduce: flock!
*/
}
public void diskUpdate(FlockInfo info) {
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		throw new AboraAssertionException("Must be inside transation");
	}
	/* noop for unregistered flocks. */
	if (info == null) {
		return ;
	}
	if (info.markContentsDirty()) {
		recordUpdate(info);
	}
/*
udanax-top.st:16908:SnarfPacker methodsFor: 'shepherds'!
{void} diskUpdate: info {FlockInfo | NULL} 
	InsideTransactionFlag fluidFetch assert: 'Must be inside transation'.
	"noop for unregistered flocks."
	info == NULL ifTrue: [^VOID].
	info markContentsDirty ifTrue: [self recordUpdate: info].!
*/
}
/**
 * Turn the flock designated by info into a Pumpkin.  It should have completed all dismantle
 * actions.
 */
public void dismantleFlock(FlockInfo info) {
	info.markDismantled();
	if ( ! (info.isNew())) {
		Someone.thingToDo();
		/* Go remove this from all the forwarded locations as well. */
		(getSnarfRecord(info.snarfID())).dismantleFlock(info);
	}
/*
udanax-top.st:16915:SnarfPacker methodsFor: 'shepherds'!
{void} dismantleFlock: info {FlockInfo} 
	"Turn the flock designated by info into a Pumpkin.  It should have completed all dismantle actions."
	info markDismantled.
	info isNew ifFalse: 
		[self thingToDo.   "Go remove this from all the forwarded locations as well."
		(self getSnarfRecord: info snarfID) dismantleFlock: info].!
*/
}
/**
 * The flock is being removed from memory.  For now, this is an error
 * if the flock has been updated.  If the flock has been forgotten,
 * then it will be dismantled when next it comes in from disk.
 * Because of forwarding, there may be many FlockInfos refering
 * to the flock if it is not new.
 */
public void dropFlock(int token) {
	FlockInfo info;
	info = FlockInfo.getInfo(token);
	if (info.isNew() || (info.isForwarded())) {
		myNewFlocks.intRemove(info.index());
	}
	if ( ! (info.isNew())) {
		if ( ! (info.isForgotten())) {
			throw new AboraRuntimeException(AboraRuntimeException.ONLY_REMOVE_UNCHANGED_FLOCKS);
		}
		Stepper stomper = (myFlocks.stepperAtInt(info.flockHash()));
		for (; stomper.hasValue(); stomper.step()) {
			FlockInfo oi = (FlockInfo) stomper.fetch();
			if (oi == null) {
				continue ;
			}
			if (oi.token() == token) {
				myFlocks.wipeIntegerVar(info.flockHash(), oi);
			}
		}
		stomper.destroy();
	}
	FlockInfo.removeInfo(token);
/*
udanax-top.st:16923:SnarfPacker methodsFor: 'shepherds'!
{void} dropFlock: token {Int32}
	"The flock is being removed from memory.  For now, this is an error
	 if the flock has been updated.  If the flock has been forgotten, 
	 then it will be dismantled when next it comes in from disk.
	 Because of forwarding, there may be many FlockInfos refering
	 to the flock if it is not new."
	| info {FlockInfo} |
	info := FlockInfo getInfo: token.
	(info isNew or: [info isForwarded])
		ifTrue: [myNewFlocks intRemove: info index].
	info isNew
		ifFalse:
			[info isForgotten ifFalse: [Heaper BLAST: #OnlyRemoveUnchangedFlocks].
			(myFlocks stepperAtInt: info flockHash) forEach: 
				[:oi {FlockInfo} |
				oi token == token ifTrue: [myFlocks wipe.IntegerVar: info flockHash with: oi]]].
	FlockInfo removeInfo: token!
*/
}
/**
 * Remember that there are no more persistent pointers to the shepherd
 * represented by info.  If it gets manually deleted, dismantle it immediately.
 * If it gets garbage collected, remember to dismantle it when it comes back
 * in from the disk.
 */
public void forgetFlock(FlockInfo info) {
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		throw new AboraAssertionException("Must be inside transation");
	}
	if (info.markForgotten()) {
		recordUpdate(info);
	}
	mySnarfInfo.setForgottenFlag(info.snarfID(), true);
	Someone.thingToDo();
	/* Don't rewrite the entire flock if it has only been forgotten. */
/*
udanax-top.st:16942:SnarfPacker methodsFor: 'shepherds'!
{void} forgetFlock: info {FlockInfo} 
	"Remember that there are no more persistent pointers to the shepherd
	 represented by info.  If it gets manually deleted, dismantle it immediately.  
	 If it gets garbage collected, remember to dismantle it when it comes back 
	 in from the disk."
	InsideTransactionFlag fluidFetch assert: 'Must be inside transation'.
	info markForgotten ifTrue: [self recordUpdate: info].
	mySnarfInfo setForgottenFlag: info snarfID with: true.
	
	self thingToDo.   "Don't rewrite the entire flock if it has only been forgotten."!
*/
}
/**
 * Return the starting object for the entire backend.  This will be the 0th
 * flock in the first snarf following the snarfInfo tables.
 */
public Turtle getInitialFlock() {
	SnarfHandler handler;
	XnReadStream stream;
	Rcvr rcvr;
	String protocol;
	String cookbook;
	Agenda agenda;
	if (myTurtle != null) {
		return myTurtle;
	}
	handler = getReadHandler(mySnarfInfo.snarfInfoCount());
	rcvr = TextyXcvrMaker.makeReader((stream = handler.readStream(0)));
	protocol = rcvr.receiveString();
	cookbook = rcvr.receiveString();
	rcvr.destroy();
	stream.destroy();
	releaseReadHandler(handler);
	myXcvrMaker = ProtocolBroker.diskProtocol(protocol);
	myBook = Cookbook.makeString(cookbook);
	/* Removed protocol.delete(); */
	/* Removed cookbook.delete(); */
	myTurtle = (Turtle) (getFlock(mySnarfInfo.snarfInfoCount(), 1));
	myTurtle.setProtocol(myXcvrMaker, myBook);
	myNextHash = myTurtle.counter();
	Someone.knownBug();
	/* this agendaItem stepping should get done, but right now it ends up happening before the backend is initialized /ravi/10/22/92/ */
	/* agenda := myTurtle fetchAgenda.
	agenda ~~ NULL ifTrue:
		[InsideAgenda fluidBind: true during:
			[[myTurtle getAgenda step] whileTrue]]. */
	destroyAbandoned();
	return myTurtle;
/*
udanax-top.st:16954:SnarfPacker methodsFor: 'shepherds'!
{Turtle} getInitialFlock
	"Return the starting object for the entire backend.  This will be the 0th
	 flock in the first snarf following the snarfInfo tables."
	| handler {SnarfHandler}
	  stream {XnReadStream} rcvr {Rcvr} 
	  protocol {char star} cookbook {char star} agenda {Agenda} |
	myTurtle ~~ NULL
		ifTrue: [^myTurtle].
		
	handler _ self getReadHandler: mySnarfInfo snarfInfoCount.
	rcvr _ TextyXcvrMaker makeReader: (stream _ handler readStream: Int32Zero).
	protocol _ rcvr receiveString.
	cookbook _ rcvr receiveString.
	rcvr destroy.
	stream destroy.
	self releaseReadHandler: handler.
	myXcvrMaker _ ProtocolBroker diskProtocol: protocol.	
	myBook _ Cookbook make.String: cookbook.
	protocol delete.
	cookbook delete.
	myTurtle _ (self getFlock: mySnarfInfo snarfInfoCount with: 1) cast: Turtle.
	myTurtle setProtocol: myXcvrMaker with: myBook.
	myNextHash _ myTurtle counter.
	self knownBug. "this agendaItem stepping should get done, but right now it ends up happening before the backend is initialized /ravi/10/22/92/"
	"agenda := myTurtle fetchAgenda.
	agenda ~~ NULL ifTrue:
		[InsideAgenda fluidBind: true during:
			[[myTurtle getAgenda step] whileTrue]]."
	self destroyAbandoned.
	^myTurtle!
*/
}
/**
 * Shepherds use a sequence number for their hash.  Return the next one
 * and increment.  This should actually spread the hashes.
 */
public int nextHashForEqual() {
	if (myNextHash == null) {
		throw new AboraRuntimeException(AboraRuntimeException.UNINITIALIZED_PACKER);
	}
	myNextHash.increment();
	/*  skip sequence numbers for the many object allocated
	at backend creation time that are likely to still be around. */
	if ((myNextHash.count() & 134217727) == 0) {
		myNextHash.setCount(myNextHash.count() + 100000);
	}
	return myNextHash.count();
/*
udanax-top.st:16986:SnarfPacker methodsFor: 'shepherds'!
{UInt32} nextHashForEqual
	"Shepherds use a sequence number for their hash.  Return the next one
	and increment.  This should actually spread the hashes."
	 
	myNextHash == NULL ifTrue: [Heaper BLAST: #UninitializedPacker].
	myNextHash increment.
	" skip sequence numbers for the many object allocated
	at backend creation time that are likely to still be around."
	(myNextHash count bitAnd: 134217727) == UInt32Zero
		ifTrue: [myNextHash setCount: myNextHash count + 100000].
	^myNextHash count DOTasLong!
*/
}
/**
 * There are now persistent pointers to the shepherd help by info.
 */
public void rememberFlock(FlockInfo info) {
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		throw new AboraAssertionException("Must be inside transation");
	}
	if (info.markRemembered()) {
		recordUpdate(info);
	}
/*
udanax-top.st:16998:SnarfPacker methodsFor: 'shepherds'!
{void} rememberFlock: info {FlockInfo} 
	"There are now persistent pointers to the shepherd help by info."
	InsideTransactionFlag fluidFetch assert: 'Must be inside transation'.
	info markRemembered ifTrue: [self recordUpdate: info]!
*/
}
/**
 * Do nothing
 */
public void storeAlmostNewShepherd(Abraham shep) {
/*
udanax-top.st:17004:SnarfPacker methodsFor: 'shepherds'!
{void} storeAlmostNewShepherd: shep {Abraham unused} 
	"Do nothing"!
*/
}
/**
 * A turtle just got created!!  Write out a pseudo-forwarder that has all the protocol
 * information encoded in the snarfID and index.
 */
public void storeInitialFlock(Abraham turtle, XcvrMaker protocol, Cookbook cookbook) {
	SnarfHandler handler;
	int length;
	Xmtr xmtr;
	XnWriteStream stream;
	myTurtle = (Turtle) turtle;
	if ( ! (turtle.fetchInfo() == null)) {
		throw new AboraAssertionException("Must not have an info yet");
	}
	handler = SnarfHandler.make((myUrdiView.makeErasingHandle(mySnarfInfo.snarfInfoCount())));
	handler.initializeSnarf();
	handler.allocateCells(1);
	length = (protocol.id().length()) + (cookbook.id().length()) + 20;
	Someone.hack();
	/* The extra 20 is not a very good measure of overhead. */
	handler.allocate(0, length);
	stream = handler.writeStream(0);
	xmtr = TextyXcvrMaker.makeWriter(stream);
	xmtr.sendString(protocol.id());
	xmtr.sendString(cookbook.id());
	xmtr.destroy();
	stream.destroy();
	mySnarfInfo.setSpaceLeft(handler.snarfID(), handler.spaceLeft());
	handler.destroy();
	myBook = cookbook;
	myXcvrMaker = protocol;
	commitView();
	storeNewFlock(turtle);
/*
udanax-top.st:17008:SnarfPacker methodsFor: 'shepherds'!
{void} storeInitialFlock: turtle {Abraham} with: protocol {XcvrMaker} with: cookbook {Cookbook}
	"A turtle just got created!!  Write out a pseudo-forwarder that has all the protocol information encoded in the snarfID and index."
	| handler {SnarfHandler} length {Int32} xmtr {Xmtr} stream {XnWriteStream} |
	myTurtle _ turtle cast: Turtle.
	turtle fetchInfo == NULL assert: 'Must not have an info yet'.
	handler _ SnarfHandler make: (myUrdiView makeErasingHandle: mySnarfInfo snarfInfoCount).
	handler initializeSnarf.
	handler allocateCells: 1.
	length _ (String strlen: protocol id) + (String strlen: cookbook id) + 20.  self hack.  "The extra 20 is not a very good measure of overhead."
	handler at: Int32Zero allocate: length.
	stream _ handler writeStream: IntegerVar0.
	xmtr _ TextyXcvrMaker makeWriter: stream.
	xmtr sendString: protocol id.
	xmtr sendString: cookbook id.
	xmtr destroy.
	stream destroy.
	mySnarfInfo setSpaceLeft: handler snarfID with: handler spaceLeft.
	handler destroy.
	myBook _ cookbook.
	myXcvrMaker _ protocol.
	self commitView.
	self storeNewFlock: turtle.!
*/
}
/**
 * Shep just got created!! On some later commit, assign it to a snarf
 * and write it to the disk.
 */
public void storeNewFlock(Abraham shep) {
	FlockInfo info;
	int newIndex;
	if ( ! (shep.fetchInfo() == null)) {
		throw new AboraAssertionException("Must not have an info yet");
	}
	/* Put the flock at the next available location in myNewFlocks. */
	newIndex = myNewFlocks.highestIndex() + 1;
	if (newIndex < myLastNewCount) {
		myLastNewCount = newIndex;
	}
	info = FlockInfo.make(shep, newIndex);
	myNewFlocks.intIntroduce(newIndex, info);
	shep.flockInfo(info);
/*
udanax-top.st:17032:SnarfPacker methodsFor: 'shepherds'!
{void} storeNewFlock: shep {Abraham} 
	"Shep just got created!! On some later commit, assign it to a snarf 
	and write it to the disk."
	| info {FlockInfo} newIndex {IntegerVar} |
	shep fetchInfo == NULL assert: 'Must not have an info yet'.
	"Put the flock at the next available location in myNewFlocks."
	newIndex _ myNewFlocks highestIndex + 1.
	newIndex < myLastNewCount ifTrue: [ myLastNewCount _ newIndex ].
	info _ FlockInfo make: shep with: newIndex.
	myNewFlocks atInt: newIndex introduce: info.
	shep flockInfo: info.!
*/
}
/**
 * If something is already imaged at that location, then return it. If there is already
 * an existing stub with the same hash at a different location, follow them till we
 * know that they are actually different objects.
 */
public Abraham fetchCanonical(int hash, int snarfID, int index) {
	Stepper flockStep;
	Stepper stomper = 
	/* myFlocks may have several FlockInfos for the same flock if the flocks
	 has been forwarded.  The actual location of the flock is determined by 
	 the flockInfo that the shepherd points at. */
	(flockStep = myFlocks.stepperAtInt(hash));
	for (; stomper.hasValue(); stomper.step()) {
		FlockInfo info = (FlockInfo) stomper.fetch();
		if (info == null) {
			continue ;
		}
		if (info != null && (info.snarfID() == snarfID && (info.index() == index))) {
			flockStep.destroy();
			return info.fetchShepherd();
		}
	}
	stomper.destroy();
	/* Didn't find an info pointing to the same disk location, so resolve infos
	 with the same hash to avoid forwarder aliasing. */
	flockStep = myFlocks.stepperAtInt(hash);
	if (flockStep.hasValue()) {
		FlockLocation newLoc;
		FlockLocation loc;
		SnarfHandler handler;
		loc = FlockLocation.make(snarfID, index);
		newLoc = null;
		while ((newLoc = (handler = getReadHandler(loc.snarfID())).fetchForward(loc.index())) != null) {
			releaseReadHandler(handler);
			loc = newLoc;
		}
		releaseReadHandler(handler);
		Stepper stomper2 = flockStep;
		for (; stomper2.hasValue(); stomper2.step()) {
			FlockInfo info1 = (FlockInfo) stomper2.fetch();
			if (info1 == null) {
				continue ;
			}
			FlockInfo newInfo;
			if (info1 != null) {
				newInfo = resolveLocation(info1);
				if (loc.snarfID() == newInfo.snarfID() && (loc.index() == newInfo.index())) {
					flockStep.destroy();
					return newInfo.fetchShepherd();
				}
			}
		}
		stomper2.destroy();
	}
	return null;
/*
udanax-top.st:17047:SnarfPacker methodsFor: 'stubs'!
{Abraham} fetchCanonical: hash {UInt32} with: snarfID {SnarfID} with: index {Int32}
	"If something is already imaged at that location, then return it. If there is already
	 an existing stub with the same hash at a different location, follow them till we 
	 know that they are actually different objects."
	| flockStep {Stepper} |
	"myFlocks may have several FlockInfos for the same flock if the flocks
	 has been forwarded.  The actual location of the flock is determined by 
	 the flockInfo that the shepherd points at."
	(flockStep _ myFlocks stepperAtInt: hash) forEach: 
		[:info {FlockInfo} |
		(info  ~~ NULL  and: [info snarfID == snarfID and: [info index == index]])
			ifTrue: [
				flockStep destroy.
				^info fetchShepherd]].
	"Didn't find an info pointing to the same disk location, so resolve infos
	 with the same hash to avoid forwarder aliasing."
	flockStep _ myFlocks stepperAtInt: hash.
	flockStep hasValue ifTrue: 
		[| newLoc {FlockLocation} loc {FlockLocation} handler {SnarfHandler} |
		loc _ FlockLocation make: snarfID with: index.
		newLoc _ NULL.
		[(newLoc _ (handler _ self getReadHandler: loc snarfID) fetchForward: loc index) ~~ NULL]
			whileTrue: [self releaseReadHandler: handler.
					loc _ newLoc].
		self releaseReadHandler: handler.
		flockStep forEach: 
			[:info {FlockInfo} |
			| newInfo {FlockInfo} |
			info ~~NULL ifTrue:[
				newInfo _ self resolveLocation: info.
				(loc snarfID == newInfo snarfID and: [loc index == newInfo index])
					ifTrue: [
						flockStep destroy.
						^newInfo fetchShepherd]]]].
	^NULL!
*/
}
/**
 * Retrieve from the disk the flock at index within the specified snarf.  Since
 * stubs are canonical, and this only gets called by stubs, the existing stub will
 * *become* the shepherd for the flock.
 */
public void makeReal(FlockInfo info) {
	Abraham stub;
	SnarfHandler handler;
	FlockLocation loc;
	stub = info.getShepherd();
	if ( ! (stub.isStub())) {
		throw new AboraAssertionException("Only stubs can be made real");
	}
	try {
		/* myInsideCommit _ true. */
		/* to prevent purge during reification */
		handler = getReadHandler(info.snarfID());
		loc = handler.fetchForward(info.index());
		if (loc == null) {
			int oldHash;
			XnReadStream stream;
			Rcvr rcvr;
			oldHash = stub.hashForEqual();
			(rcvr = makeRcvr((stream = handler.readStream(info.index())))).receiveInto(stub);
			rcvr.destroy();
			stream.destroy();
			if ( ! (stub.hashForEqual() == oldHash)) {
				throw new AboraAssertionException("Hash must not change");
			}
			info.setSize((handler.flockSize(info.index())));
			/* Receiving the flock has cleared its info, so put it back */
			stub.flockInfo(info);
		}
		else {
			/* Forwarded.  Register stub at the new location.  We leave the old info in place so
			that later references through the forwarder. */
			addInfo((FlockInfo.make(stub.getInfo(), loc.snarfID(), loc.index())), stub);
		}
		releaseReadHandler(handler);
		handler = null;
	}
	finally {
		SnarfPacker.bombResetCommit(this);
	}
	/* If the flock is forwarded, then the first instantiate will just change the location of the stub.  Retry. */
	if (info.getShepherd().isStub()) {
		makeReal(stub.getInfo());
	}
/*
udanax-top.st:17084:SnarfPacker methodsFor: 'stubs'!
{void} makeReal: info {FlockInfo}
	"Retrieve from the disk the flock at index within the specified snarf.  Since
	 stubs are canonical, and this only gets called by stubs, the existing stub will 
	 *become* the shepherd for the flock."
	| stub {Abraham} handler {SnarfHandler} loc {FlockLocation | NULL} |
	stub _ info getShepherd.
	stub isStub assert: 'Only stubs can be made real'.
	["myInsideCommit _ true." "to prevent purge during reification"
	handler _ self getReadHandler: info snarfID.
	loc _ handler fetchForward: info index.
	loc == NULL
		ifTrue: 
			[| oldHash {UInt32} stream {XnReadStream} rcvr {Rcvr} |
			oldHash _ stub hashForEqual.
			(rcvr _ self makeRcvr: (stream _ handler readStream: info index)) receiveInto: stub.
			rcvr destroy.
			stream destroy.
			stub hashForEqual == oldHash assert: 'Hash must not change'.
			info setSize: (handler flockSize: info index).
			"Receiving the flock has cleared its info, so put it back"
			stub flockInfo: info]
		ifFalse:
			["Forwarded.  Register stub at the new location.  We leave the old info in place so
			that later references through the forwarder."
			self addInfo: (FlockInfo make: stub getInfo with: loc snarfID with: loc index) with: stub].
	self releaseReadHandler: handler.
	handler _ NULL]
	valueNowOrOnUnwindDo: (SnarfPacker bomb.ResetCommit: self).
	"If the flock is forwarded, then the first instantiate will just change the location of the stub.  Retry."
	info getShepherd isStub ifTrue: [self makeReal: stub getInfo]!
*/
}
public void registerStub(Abraham shep, int snarfID, int index) {
	if ( ! (shep.isStub())) {
		throw new AboraAssertionException("Must be stub");
	}
	addInfo((FlockInfo.remembered(shep, snarfID, index)), shep);
/*
udanax-top.st:17116:SnarfPacker methodsFor: 'stubs'!
{void} registerStub: shep {Abraham} with: snarfID {SnarfID} with: index {Int32}
	shep isStub assert: 'Must be stub'.
	self addInfo: (FlockInfo remembered: shep with: snarfID with: index) with: shep!
*/
}
/**
 * Add another flockInfo object to myFlocks with info about another location for shep.
 */
public void addInfo(FlockInfo info, Abraham shep) {
	myFlocks.intStore(shep.hashForEqual(), info);
	shep.flockInfo(info);
/*
udanax-top.st:17122:SnarfPacker methodsFor: 'internals'!
{void} addInfo: info {FlockInfo} with: shep {Abraham}
	"Add another flockInfo object to myFlocks with info about another location for shep."
	myFlocks atInt: shep hashForEqual store: info.
	shep flockInfo: info!
*/
}
/**
 * Send the snarf over a transmitter into a stream that just counts the bytes put
 * into it.
 */
public int computeSize(Abraham flock) {
	TransferSpecialist specialist;
	XnWriteStream counter;
	Xmtr xmtr;
	int size;
	counter = CountStream.make();
	specialist = DiskCountSpecialist.make(myBook);
	xmtr = myXcvrMaker.makeXmtr(specialist, counter);
	xmtr.sendHeaper(flock);
	size = ((CountStream) counter).size();
	xmtr.destroy();
	/* specialist destroy. */
	counter.destroy();
	return size;
/*
udanax-top.st:17128:SnarfPacker methodsFor: 'internals'!
{Int32} computeSize: flock {Abraham} 
	"Send the snarf over a transmitter into a stream that just counts the bytes put 
	into it."
	
	| specialist {TransferSpecialist} counter {XnWriteStream} xmtr {Xmtr} size {Int32} | 
	counter _ CountStream make.
	specialist _ DiskCountSpecialist make: myBook.
	xmtr _ myXcvrMaker makeXmtr: specialist with: counter.
	xmtr sendHeaper: flock.
	size _ (counter cast: CountStream) size.
	xmtr destroy.
	"specialist destroy."
	counter destroy.
	^size!
*/
}
/**
 * Return the current urdiView.
 */
public UrdiView currentView() {
	return myUrdiView;
/*
udanax-top.st:17143:SnarfPacker methodsFor: 'internals'!
{UrdiView} currentView
	"Return the current urdiView."
	
	^myUrdiView!
*/
}
/**
 * Destroy all forgotten flocks that are no longer in memory.
 */
public void destroyAbandoned() {
	if (true) {
		return ;
	}
	AboraSupport.smalltalkOnly();
	{
		AboraSupport.logger.print("+");
	}
	for (int snarfID = mySnarfInfo.snarfInfoCount(); snarfID < mySnarfInfo.snarfCount(); snarfID ++ ) {
		boolean reset;
		reset = false;
		/* In case we run into unforgettable objects. */
		while (mySnarfInfo.getForgottenFlag(snarfID)) {
			/* Clear the flag first so we'll catch newly forgotten shepherds. */
			mySnarfInfo.setForgottenFlag(snarfID, false);
			Stepper stomper = (forgottenFlocks(snarfID)).stepper();
			for (; stomper.hasValue(); stomper.step()) {
				IntegerPos iD = (IntegerPos) stomper.fetch();
				if (iD == null) {
					continue ;
				}
				int index;
				index = iD.asIntegerVar();
				if ((fetchInMemory(snarfID, index)) == null) {
					(getFlock(snarfID, index)).destroy();
					endConsistent(0);
				}
				else {
					reset = true;
				}
			}
			stomper.destroy();
		}
		if (reset) {
			mySnarfInfo.setForgottenFlag(snarfID, true);
		}
	}
/*
udanax-top.st:17148:SnarfPacker methodsFor: 'internals'!
{void} destroyAbandoned
	"Destroy all forgotten flocks that are no longer in memory."
	 
	true ifTrue: [^VOID].
	[cerr << '+'] smalltalkOnly.
	mySnarfInfo snarfInfoCount almostTo: mySnarfInfo snarfCount do:
		[:snarfID {Int32} |
		| reset {BooleanVar} |
		reset _ false.      "In case we run into unforgettable objects."
		[mySnarfInfo getForgottenFlag: snarfID] whileTrue:
			["Clear the flag first so we'll catch newly forgotten shepherds."
			mySnarfInfo setForgottenFlag: snarfID with: false.
			(self forgottenFlocks: snarfID) stepper forEach: 
				[:iD {IntegerPos} | 
				| index {Int32} |
				index _ iD asIntegerVar DOTasLong.
				(self fetchInMemory: snarfID with: index) == NULL
					ifTrue:
						[(self getFlock: snarfID with: index) destroy.
						self endConsistent: IntegerVarZero]
					ifFalse: [reset _ true]]].
		reset ifTrue: [mySnarfInfo setForgottenFlag: snarfID with: true]].!
*/
}
/**
 * Shep has grown too large for its current place.  Treat it as just a new flock and give it
 * another place.
 */
public void forwardFlock(Abraham shep) {
	if ( ! ( ! (shep.isEqual(Pumpkin.make())))) {
		throw new AboraAssertionException("Only forward real Flocks");
	}
	shep.getInfo().forward(myNewFlocks.highestIndex() + 1);
	/* So a weak dropFlock will do the right thing. */
	myNewFlocks.intIntroduce(myNewFlocks.highestIndex() + 1, shep.getInfo());
/*
udanax-top.st:17171:SnarfPacker methodsFor: 'internals'!
{void} forwardFlock: shep {Abraham} 
	"Shep has grown too large for its current place.  Treat it as just a new flock and give it another place."
	(shep isEqual: Pumpkin make) not assert: 'Only forward real Flocks'.
	shep getInfo forward: myNewFlocks highestIndex DOTasLong + 1. "So a weak dropFlock will do the right thing."
	myNewFlocks atInt: myNewFlocks highestIndex + 1 introduce: shep getInfo.!
*/
}
public SpecialistRcvr makeRcvr(XnReadStream readStream) {
	return myXcvrMaker.makeRcvr((DiskSpecialist.make(myBook, this)), readStream);
/*
udanax-top.st:17178:SnarfPacker methodsFor: 'internals'!
{SpecialistRcvr} makeRcvr: readStream {XnReadStream}
	^myXcvrMaker makeRcvr: (DiskSpecialist make: myBook with: self) with: readStream!
*/
}
public SpecialistXmtr makeXmtr(XnWriteStream writeStream) {
	return myXcvrMaker.makeXmtr((DiskSpecialist.make(myBook, this)), writeStream);
/*
udanax-top.st:17181:SnarfPacker methodsFor: 'internals'!
{SpecialistXmtr} makeXmtr: writeStream {XnWriteStream}
	^myXcvrMaker makeXmtr: (DiskSpecialist make: myBook with: self) with: writeStream!
*/
}
public void setHashCounter(Counter aCounter) {
	myNextHash = aCounter;
/*
udanax-top.st:17184:SnarfPacker methodsFor: 'internals'!
{void} setHashCounter: aCounter {Counter}
	myNextHash _ aCounter!
*/
}
public void testNewFlocks() {
	Stepper stomper = myNewFlocks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FlockInfo info = (FlockInfo) stomper.fetch();
		if (info == null) {
			continue ;
		}
	}
	stomper.destroy();
/*
udanax-top.st:17187:SnarfPacker methodsFor: 'internals'!
{void} testNewFlocks
	myNewFlocks stepper forEach: [:info {FlockInfo} | ]!
*/
}
public void beginConsistent(int dirtyFlocks) {
	checkInfos();
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		int dirtySnarfs;
		int bytesPerSnarf;
		if (dirtyFlocks == -1) {
			dirtySnarfs = 10;
		}
		else {
			dirtySnarfs = Math.min(dirtyFlocks, 20);
		}
		bytesPerSnarf = myUrdiView.getDataSizeOfSnarf(0);
		/* Now the dirtySnarfs from new flocks (including the mapCell). */
		dirtySnarfs = dirtySnarfs + (myNewFlocks.count() * (8 + myNewEstimate) / bytesPerSnarf);
		/* Now the dirtySnarfs from changed flocks. */
		dirtySnarfs = dirtySnarfs + mySnarfMap.count();
		/* Now a buffer for good measure. */
		dirtySnarfs = dirtySnarfs + SpareStageSpace.cruftedSnarfsGuess();
		if (dirtySnarfs >= myUrdi.usableStages()) {
			makePersistent();
		}
	}
/*
udanax-top.st:17192:SnarfPacker methodsFor: 'transactions'!
{void} beginConsistent: dirtyFlocks {IntegerVar} 
	self checkInfos.
	InsideTransactionFlag fluidFetch
		ifFalse:
			[| dirtySnarfs {Int32} bytesPerSnarf {Int32} |
			dirtyFlocks = -1
				ifTrue: [dirtySnarfs _ 10]
				ifFalse: [dirtySnarfs _ dirtyFlocks DOTasLong min: 20].
			bytesPerSnarf _ myUrdiView getDataSizeOfSnarf: Int32Zero.
		"Now the dirtySnarfs from new flocks (including the mapCell)."
			dirtySnarfs _ dirtySnarfs + (myNewFlocks count * 8 + myNewEstimate // bytesPerSnarf) DOTasLong.  
		"Now the dirtySnarfs from changed flocks."
			dirtySnarfs _ dirtySnarfs + mySnarfMap count DOTasLong.
		"Now a buffer for good measure."
			dirtySnarfs _ dirtySnarfs + SpareStageSpace cruftedSnarfsGuess.
			
			dirtySnarfs >= myUrdi usableStages ifTrue: [self makePersistent]]!
*/
}
public void endConsistent(int dirty) {
	Agenda agenda;
	if (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue()) {
		return ;
	}
	for (int i = 
	/* Measure all the new flocks from the previous consistent block. */
	myLastNewCount; i <= myNewFlocks.highestIndex(); i ++ ) {
		FlockInfo info;
		info = (FlockInfo) (myNewFlocks.intFetch(i));
		if (info != null) {
			Abraham shep;
			shep = info.fetchShepherd();
			if (shep != null) {
				int size;
				size = computeSize(shep);
				info.setSize(size);
				myNewEstimate = myNewEstimate + size
				/* + (size // 10) */
				;
			}
		}
	}
	myLastNewCount = myNewFlocks.highestIndex() + 1;
	myConsistentCount = myConsistentCount + 1;
	Someone.hack();
	/* Do all agenda items before any destroys so we don't need to worry about pointers
	 from Agenda Items into the data structures. */
	if (((Boolean) InsideAgenda.fluidFetch()).booleanValue()) {
		return ;
	}
	agenda = myTurtle.fetchAgenda();
	if (agenda != null) {
		Object insideAgendaOldValue = AboraBlockSupport.enterFluidBindDuring(InsideAgenda, true);
		try {
			while (agenda.step());
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InsideAgenda, insideAgendaOldValue);
		}
	}
	/* Now dismantled destroyed flocks. */
	if (myDestroyedFlocks.isEmpty()) {
		return ;
	}
	Object insideAgendaOldValue1 = AboraBlockSupport.enterFluidBindDuring(InsideAgenda, true);
	try {
		while ( ! (myDestroyedFlocks.isEmpty())) {
			Abraham shep1;
			/* The count of the table is used as the index to insert things at, so it get's manipulated carefully here. */
			/* The destroy table is LIFO so that recursive destruction is depth first (to queue size). */
			shep1 = (Abraham) (myDestroyedFlocks.intGet(myDestroyedFlocks.count() - 1));
			myDestroyedFlocks.intRemove(myDestroyedFlocks.count() - 1);
			if (shep1.getInfo().isForgotten()) {
				shep1.dismantle();
			}
			myDestroyCount = myDestroyCount + 1;
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InsideAgenda, insideAgendaOldValue1);
	}
	checkInfos();
/*
udanax-top.st:17210:SnarfPacker methodsFor: 'transactions'!
{void} endConsistent: dirty {IntegerVar unused} 
	
	| agenda {Agenda | NULL} |
	InsideTransactionFlag fluidFetch ifTrue: [^VOID].
	"Measure all the new flocks from the previous consistent block."
	myLastNewCount to: myNewFlocks highestIndex do:
		[:i {IntegerVar} |
		| info {FlockInfo} |
		info _ (myNewFlocks intFetch: i) cast: FlockInfo.
		info ~~ NULL ifTrue:
			[| shep {Abraham} |
			shep _ info fetchShepherd.
			shep ~~ NULL ifTrue:
				[| size {Int32} |
				size _ self computeSize: shep.
				info setSize: size.
				myNewEstimate _ myNewEstimate + size "+ (size // 10)"]]].
	myLastNewCount _ myNewFlocks highestIndex + 1.
	myConsistentCount _ myConsistentCount + 1.
	
	self hack.
	"Do all agenda items before any destroys so we don't need to worry about pointers
	 from Agenda Items into the data structures."
	InsideAgenda fluidFetch ifTrue: [^VOID].
	agenda _ myTurtle fetchAgenda.
	agenda ~~ NULL ifTrue: 
		[InsideAgenda fluidBind: true during:
			[[agenda step] whileTrue]].
	"Now dismantled destroyed flocks."
	myDestroyedFlocks isEmpty ifTrue: [^VOID]. 
	InsideAgenda fluidBind: true during:
		[[myDestroyedFlocks isEmpty] whileFalse:
			[| shep {Abraham} |
			"The count of the table is used as the index to insert things at, so it get's manipulated carefully here."
			"The destroy table is LIFO so that recursive destruction is depth first (to queue size)."
			shep _ (myDestroyedFlocks intGet: myDestroyedFlocks count - 1) cast: Abraham.
			myDestroyedFlocks intRemove: myDestroyedFlocks count - 1.
			shep getInfo isForgotten ifTrue: [shep dismantle].
			myDestroyCount _ myDestroyCount + 1]].
	self checkInfos.!
*/
}
public boolean insideCommit() {
	return myInsideCommit;
/*
udanax-top.st:17252:SnarfPacker methodsFor: 'transactions'!
{BooleanVar} insideCommit
	^myInsideCommit!
*/
}
/**
 * The virtual image in memory is now in a consistent state. Write the image of
 * all changed or new Shepherds out to the disk in a single atomic action.  The
 * atomicity only happens on top of a real Urdi, however.
 */
public void makePersistent() {
	checkInfos();
	try {
		myInsideCommit = true;
		/* Note which flocks still fit in their snarfs, and forwards ones that don't */
		refitFlocks();
		for (int i = 
		/* Assign all new and migrating flocks to a snarf in a GC safe fashion. */
		0; i <= myNewFlocks.highestIndex(); i ++ ) {
			FlockInfo info;
			info = (FlockInfo) (myNewFlocks.intFetch(i));
			/* IF we GC'd, flocks and their infos might have been removed. */
			if (info != null) {
				Abraham shep;
				/* This might be the only strong pointer to the object!! */
				info.markShepNull();
				shep = info.fetchShepherd();
				if (shep != null) {
					assignSnarf(shep);
				}
			}
		}
		/* Write out all the changes into URDI buffers. */
		flushFlocks();
		myNewFlocks.destroy();
		myNewFlocks = IntegerTable.make(500);
		commitView();
		AboraSupport.logger.print(".");
		myNewEstimate = 0;
	}
	finally {
		SnarfPacker.bombResetCommit(this);
	}
	checkInfos();
/*
udanax-top.st:17255:SnarfPacker methodsFor: 'transactions'!
{void} makePersistent
	"The virtual image in memory is now in a consistent state. Write the image of 
	all changed or new Shepherds out to the disk in a single atomic action.  The 
	atomicity only happens on top of a real Urdi, however."
	
	self checkInfos.
	[myInsideCommit _ true.
	"Note which flocks still fit in their snarfs, and forwards ones that don't"
	self refitFlocks.
	"Assign all new and migrating flocks to a snarf in a GC safe fashion."
	IntegerVarZero to: myNewFlocks highestIndex do: 
		[:i {IntegerVar} |
		| info {FlockInfo} |
		info _ (myNewFlocks intFetch: i) cast: FlockInfo.
		"IF we GC'd, flocks and their infos might have been removed."
		info ~~ NULL ifTrue: 
			[| shep {Abraham} |  "This might be the only strong pointer to the object!!"
			info markShepNull.
			shep _ info fetchShepherd.
			shep ~~ NULL ifTrue: [self assignSnarf: shep]]].
	"Write out all the changes into URDI buffers."
	self flushFlocks.
	myNewFlocks destroy.
	myNewFlocks _ IntegerTable make: 500.
	self commitView.
	[Transcript show: '.'] smalltalkOnly.
	myNewEstimate _ IntegerVarZero]
		valueNowOrOnUnwindDo:
			(SnarfPacker bomb.ResetCommit: self).
	self checkInfos.!
*/
}
/**
 * Flush everything out to disk and remove all purgeable imaged
 * objects from memory.
 */
public void purge() {
	if (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue()) {
		return ;
	}
	makePersistent();
	purgeClean(true);
/*
udanax-top.st:17286:SnarfPacker methodsFor: 'transactions'!
{void} purge
	"Flush everything out to disk and remove all purgeable imaged
	 objects from memory."
	
	InsideTransactionFlag fluidFetch ifTrue: [^VOID].
	self makePersistent.
	self purgeClean: true!
*/
}
/**
 * purge all shepherds that are currently clean, not locked, not dirty,
 * and purgeable.  Purging just turns them into stubs, freeing all their
 * flocks.  Garbage collection can clean up the flocks and any stubs no
 * longer pointed to by something in memory.  Because infos for new
 * flocks don't appear in myFlocks, this will not throw out any newFlocks
 * (which will be marked dirty anyway).  For each FlockInfo, we check
 * that its flock refers to that exact instance to get correct information
 * about its dirty state.
 */
public void purgeClean(boolean noneLocked) {
	PrimPtrTable stackPtrs;
	if (myInsideCommit) {
		return ;
	}
	try {
		myInsideCommit = true;
		/* to prevent recursive call */
		AboraSupport.logger.print("Starting purge...");
		if (noneLocked) {
			stackPtrs = PrimPtrTable.make(1);
		}
		else {
			stackPtrs = StackExaminer.pointersOnStack();
		}
		Stepper stomper = myFlocks.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FlockInfo info = (FlockInfo) stomper.fetch();
			if (info == null) {
				continue ;
			}
			Abraham shep;
			shep = info.fetchShepherd();
			if (shep != null && (shep.fetchInfo() == info && ( ! shep.isStub() && ((stackPtrs.fetch(shep.asOop())) == null && (shep.isPurgeable() && ( ! info.isDirty())))))) {
				shep.becomeStub();
			}
			/* Removed translateOnly */
		}
		stomper.destroy();
	}
	finally {
		SnarfPacker.bombResetCommit(this);
	}
	if ( ! (noneLocked)) {
		myRepairer.setMustPurge();
	}
	AboraSupport.logger.print("done.");
	AboraSupport.logger.println();
/*
udanax-top.st:17294:SnarfPacker methodsFor: 'transactions'!
{void} purgeClean: noneLocked {BooleanVar default: false}
	"purge all shepherds that are currently clean, not locked, not dirty,
	 and purgeable.  Purging just turns them into stubs, freeing all their 
	 flocks.  Garbage collection can clean up the flocks and any stubs no 
	 longer pointed to by something in memory.  Because infos for new 
	 flocks don't appear in myFlocks, this will not throw out any newFlocks 
	 (which will be marked dirty anyway).  For each FlockInfo, we check
	 that its flock refers to that exact instance to get correct information
	 about its dirty state."
	 | stackPtrs {PrimPtrTable} |
	myInsideCommit ifTrue: [^VOID].
	[myInsideCommit _ true. "to prevent recursive call"
	[Transcript show: 'Starting purge...'] smalltalkOnly.
	noneLocked
		ifTrue: [stackPtrs _ PrimPtrTable make: 1]
		ifFalse: [stackPtrs _ StackExaminer pointersOnStack].
	myFlocks stepper forEach:
		[:info {FlockInfo} |
		| shep {Abraham} |
		shep _ info fetchShepherd.
		[(shep ~~ NULL
			and: [shep fetchInfo == info
			and: [shep isStub not
			and: [(stackPtrs fetch: shep asOop) == NULL
			and: [shep isPurgeable
			and: [info isDirty not]]]]])
			ifTrue: [shep becomeStub]] smalltalkOnly.
		'if (shep && shep->fetchInfo() == info && !!shep->isStub() && (stackPtrs->fetch((Int32)(void*)shep) == NULL) && shep->isPurgeable() && !!info->isDirty()) {
			shep->becomeStub();
			}' translateOnly.]]
	valueNowOrOnUnwindDo:
		(SnarfPacker bomb.ResetCommit: self).
	noneLocked ifFalse: [myRepairer setMustPurge].
	[Transcript show: 'done.'; cr] smalltalkOnly!
*/
}
/**
 * Destroy all objects imaged from this snarf.
 */
public void destruct() {
	myPurgeror.destroy();
	if ( ! (Heaper.isDestructed(mySnarfMap))) {
		Stepper stomper = mySnarfMap.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			Heaper rec = (Heaper) stomper.fetch();
			if (rec == null) {
				continue ;
			}
			rec.destroy();
		}
		stomper.destroy();
		mySnarfMap.destroy();
	}
	/* myFlocks getCategory ~= Heaper ifTrue:
		[myFlocks stepper forEach:
			[:info {FlockInfo} | 
			(Heaper isDestructed: info) ifFalse: 
				[info getShepherd flockInfo: NULL.
				info destroy]].
		myFlocks destroy].
	myNewFlocks getCategory ~= Heaper ifTrue:
		[myNewFlocks stepper forEach:
			[:info {FlockInfo} | 
			(Heaper isDestructed: info) ifFalse: 
				[info getShepherd flockInfo: NULL. 
				info destroy]].
		myNewFlocks destroy]. */
	mySnarfInfo.destroy();
	myXcvrMaker = null;
	myBook.destroy();
	myUrdiView.destroy();
	myUrdi.destroy();
	super.destruct();
/*
udanax-top.st:17331:SnarfPacker methodsFor: 'protected: destruction'!
{void} destruct
	"Destroy all objects imaged from this snarf."
	
	myPurgeror destroy.
	(Heaper isDestructed: mySnarfMap) ifFalse:
		[mySnarfMap stepper forEach: [:rec {Heaper} | rec destroy].
		mySnarfMap destroy].
	"myFlocks getCategory ~= Heaper ifTrue:
		[myFlocks stepper forEach:
			[:info {FlockInfo} | 
			(Heaper isDestructed: info) ifFalse: 
				[info getShepherd flockInfo: NULL.
				info destroy]].
		myFlocks destroy].
	myNewFlocks getCategory ~= Heaper ifTrue:
		[myNewFlocks stepper forEach:
			[:info {FlockInfo} | 
			(Heaper isDestructed: info) ifFalse: 
				[info getShepherd flockInfo: NULL. 
				info destroy]].
		myNewFlocks destroy]."
	mySnarfInfo destroy.
	myXcvrMaker _ NULL.
	myBook destroy.
	myUrdiView destroy.
	myUrdi destroy.
	super destruct!
*/
}
/**
 * Find a snarf in which to fit shep.  Then assign it to
 * that location, and mark that snarf as changed.
 */
public void assignSnarf(Abraham shep) {
	int size;
	SnarfRecord rec;
	int index;
	FlockInfo oldInfo;
	/* Migrating flocks already have a size computed.  Likewise new
	 flocks that haven't changed since they were estimated. */
	size = shep.getInfo().oldSize();
	if (shep.getInfo().isNew() && (shep.getInfo().isContentsDirty())) {
		size = (computeSize(shep));
	}
	/* Include the space for a slot in the snarf map table. */
	size = size + SnarfHandler.mapCellOverhead();
	/* Check that size fits in a snarf */
	Eric.hack();
	/* This assumes that all snarfs are the same size */
	if (size > (myUrdi.getDataSizeOfSnarf(0))) {
		throw new AboraRuntimeException(AboraRuntimeException.OVERGRAZED);
	}
	/* Check in the snarf last allocated.  Search for another (first up, then down) if it won't fit. */
	if (size > (mySnarfInfo.getSpaceLeft(myAllocationSnarf))) {
		int limitSnarf;
		int snarfID;
		/* First search upward. */
		limitSnarf = mySnarfInfo.snarfCount();
		snarfID = myAllocationSnarf + 1;
		while (snarfID < limitSnarf && (size > (mySnarfInfo.getSpaceLeft(snarfID)))) {
			snarfID = snarfID + 1;
		}
		/* Then if we didn't find space, search downward. */
		if (snarfID >= limitSnarf) {
			limitSnarf = mySnarfInfo.snarfInfoCount() - 1;
			snarfID = myAllocationSnarf - 1;
			while (snarfID > limitSnarf && (size > (mySnarfInfo.getSpaceLeft(snarfID)))) {
				snarfID = snarfID - 1;
			}
			if (snarfID <= limitSnarf) {
				throw new AboraRuntimeException(AboraRuntimeException.DISK_FULL);
			}
		}
		myAllocationSnarf = snarfID;
	}
	if ( ! (myAllocationSnarf >= mySnarfInfo.snarfInfoCount())) {
		throw new AboraAssertionException("A real snarf");
	}
	if (shep.getInfo().isForgotten()) {
		mySnarfInfo.setForgottenFlag(myAllocationSnarf, true);
	}
	rec = getSnarfRecord(myAllocationSnarf);
	/* Update the size information and such inside the per-snarf data-structure. */
	index = rec.allocate(size, shep);
	oldInfo = shep.getInfo();
	addInfo((FlockInfo.make(oldInfo, myAllocationSnarf, index)), shep);
	/* Destroy the old location. */
	if (oldInfo.isNew() || (oldInfo.isForwarded())) {
		if (oldInfo.isForwarded()) {
			myFlocks.wipeIntegerVar(oldInfo.flockHash(), oldInfo);
		}
		myNewFlocks.intWipe(oldInfo.index());
		oldInfo.destroy();
	}
	/* Remember the space is gone */
	mySnarfInfo.setSpaceLeft(myAllocationSnarf, rec.spaceLeft());
/*
udanax-top.st:17361:SnarfPacker methodsFor: 'private:'!
{void} assignSnarf: shep {Abraham}
	"Find a snarf in which to fit shep.  Then assign it to
	 that location, and mark that snarf as changed."
	
	| size {Int32} rec {SnarfRecord} index {Int32} oldInfo {FlockInfo} |
	"Migrating flocks already have a size computed.  Likewise new
	 flocks that haven't changed since they were estimated."
	size _ shep getInfo oldSize.
	(shep getInfo isNew and: [shep getInfo isContentsDirty])
		ifTrue: [size _ (self computeSize: shep)].
	"Include the space for a slot in the snarf map table."
	size _ size + SnarfHandler mapCellOverhead.
	"Check that size fits in a snarf"
	Eric hack.  "This assumes that all snarfs are the same size"
	size > (myUrdi getDataSizeOfSnarf: Int32Zero) ifTrue:
		[Heaper BLAST: #Overgrazed].
	"Check in the snarf last allocated.  Search for another (first up, then down) if it won't fit."
	size > (mySnarfInfo getSpaceLeft: myAllocationSnarf) ifTrue:
		[| limitSnarf {SnarfID} snarfID {SnarfID} |
		"First search upward."
		limitSnarf _ mySnarfInfo snarfCount.
		snarfID _ myAllocationSnarf + 1.
		[snarfID < limitSnarf and: [size > (mySnarfInfo getSpaceLeft: snarfID)]] whileTrue:
			[snarfID _ snarfID + 1].
		"Then if we didn't find space, search downward."
		snarfID >= limitSnarf ifTrue:
			[limitSnarf _ mySnarfInfo snarfInfoCount - 1.
			snarfID _ myAllocationSnarf - 1.
			[snarfID > limitSnarf and: [size > (mySnarfInfo getSpaceLeft: snarfID)]] whileTrue:
				[snarfID _ snarfID - 1].
			snarfID <= limitSnarf ifTrue: [Heaper BLAST: #DiskFull]].
		myAllocationSnarf _ snarfID].
	myAllocationSnarf >= mySnarfInfo snarfInfoCount assert: 'A real snarf'.
	shep getInfo isForgotten ifTrue: [mySnarfInfo setForgottenFlag: myAllocationSnarf with: true].
	rec _ self getSnarfRecord: myAllocationSnarf.
	"Update the size information and such inside the per-snarf data-structure."
	index _ rec allocate: size with: shep.
	oldInfo _ shep getInfo.
	self addInfo: (FlockInfo make: oldInfo with: myAllocationSnarf with: index) with: shep.
	"Destroy the old location."
	(oldInfo isNew or: [oldInfo isForwarded]) ifTrue:
		[oldInfo isForwarded ifTrue:
			[myFlocks wipe.IntegerVar: oldInfo flockHash with: oldInfo].
		myNewFlocks intWipe: oldInfo index.
		oldInfo destroy].
	"Remember the space is gone"
	mySnarfInfo setSpaceLeft: myAllocationSnarf with: rec spaceLeft!
*/
}
/**
 * Perform the sanity check of the moment.  Beware the compile cost of changing this comment.
 */
public void checkInfos() {
	/* myFlocks stepper forEach: [:info {FlockInfo} | info getShepherd].
	myNewFlocks stepper forEach: [:info {FlockInfo} | info getShepherd] */
/*
udanax-top.st:17409:SnarfPacker methodsFor: 'private:'!
{void} checkInfos
	"Perform the sanity check of the moment.  Beware the compile cost of changing this comment."
	
	"myFlocks stepper forEach: [:info {FlockInfo} | info getShepherd].
	myNewFlocks stepper forEach: [:info {FlockInfo} | info getShepherd]"!
*/
}
/**
 * Used by ResetCommit bomb
 */
public void commitState(boolean flag) {
	myInsideCommit = flag;
/*
udanax-top.st:17415:SnarfPacker methodsFor: 'private:'!
{void} commitState: flag {BooleanVar}
	"Used by ResetCommit bomb"
	myInsideCommit := flag!
*/
}
/**
 * Commit by destroying the current view and creating a new one.
 */
public void commitView() {
	UrdiView newView;
	myUrdiView.commitWrite();
	mySnarfInfo.destroy();
	mySnarfInfo = null;
	myUrdiView.becomeRead();
	newView = myUrdi.makeWriteView();
	myUrdiView.destroy();
	myUrdiView = newView;
	mySnarfInfo = SnarfInfoHandler.make(myUrdi, myUrdiView);
/*
udanax-top.st:17419:SnarfPacker methodsFor: 'private:'!
{void} commitView
	"Commit by destroying the current view and creating a new one."
	
	| newView {UrdiView} |
	myUrdiView commitWrite.
	mySnarfInfo destroy.
	mySnarfInfo _ NULL.
	myUrdiView becomeRead.
	newView _ myUrdi makeWriteView.
	myUrdiView destroy.
	myUrdiView _ newView.
	mySnarfInfo _ SnarfInfoHandler make: myUrdi with: myUrdiView!
*/
}
/**
 * Return true if the object is on disk but not in memory.
 */
public Abraham fetchInMemory(int snarfID, int index) {
	SnarfHandler handler;
	FlockLocation loc;
	XnReadStream stream;
	SpecialistRcvr rcvr;
	int hash;
	Category cat;
	handler = getReadHandler(snarfID);
	loc = handler.fetchForward(index);
	if (loc != null) {
		releaseReadHandler(handler);
		return null;
	}
	Someone.hack();
	/* This is partially reading in the flock in order to get its hash!!  Ick!! */
	stream = handler.readStream(index);
	rcvr = (SpecialistRcvr) (makeRcvr(stream));
	if ( ! ((cat = rcvr.receiveCategory()).isEqualOrSubclassOf(AboraSupport.findCategory(Abraham.class)))) {
		releaseReadHandler(handler);
		throw new AboraRuntimeException(AboraRuntimeException.NON_SHEPHERD);
	}
	/* Right now this keeps looking for an end-of-packet marker.  Grrr. */
	hash = rcvr.receiveUInt32();
	rcvr.destroy();
	stream.destroy();
	releaseReadHandler(handler);
	return fetchCanonical(hash, snarfID, index);
/*
udanax-top.st:17432:SnarfPacker methodsFor: 'private:'!
{Abraham | NULL} fetchInMemory: snarfID {SnarfID} with: index {Int32}
	"Return true if the object is on disk but not in memory."
	 
	| handler {SnarfHandler} loc {FlockLocation | NULL} stream {XnReadStream} rcvr {SpecialistRcvr} hash {UInt32} cat {Category} |
	handler _ self getReadHandler: snarfID.
	loc _ handler fetchForward: index.
	loc~~ NULL ifTrue: [self releaseReadHandler: handler.   ^NULL].
	self hack.  "This is partially reading in the flock in order to get its hash!!  Ick!!"
	stream _ handler readStream: index.
	rcvr _ (self makeRcvr: stream) cast: SpecialistRcvr.
	((cat _ rcvr receiveCategory) isEqualOrSubclassOf: Abraham) 
		ifFalse: [self releaseReadHandler: handler.   Heaper BLAST: #NonShepherd].
	"Right now this keeps looking for an end-of-packet marker.  Grrr."
	hash _ rcvr receiveUInt32.
	rcvr destroy.
	stream destroy.
	self releaseReadHandler: handler.
	^self fetchCanonical: hash with: snarfID with: index.!
*/
}
/**
 * Actually write all the changed and newly assigned flocks to the disk.
 */
public void flushFlocks() {
	TableStepper stomper = mySnarfMap.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		int index = (int) stomper.index();
		SnarfRecord rec = (SnarfRecord) stomper.fetch();
		if (rec == null) {
			continue ;
		}
		rec.flushChanges();
		mySnarfMap.intWipe(index);
		rec.destroy();
	}
	stomper.destroy();
	mySnarfMap.destroy();
	mySnarfMap = IntegerTable.make(50);
/*
udanax-top.st:17451:SnarfPacker methodsFor: 'private:'!
{void} flushFlocks
	"Actually write all the changed and newly assigned flocks to the disk."
	 
	mySnarfMap stepper forIndices: 
		[:index {IntegerVar} :rec {SnarfRecord} |
		rec flushChanges.
		mySnarfMap intWipe: index.
		rec destroy].
	mySnarfMap destroy.
	mySnarfMap _ IntegerTable make: 50.!
*/
}
/**
 * Return the set of indices to flocks in snarf snarfID that are forgotten.
 */
public MuSet forgottenFlocks(int snarfID) {
	MuSet result;
	SnarfHandler handler;
	handler = getReadHandler(snarfID);
	result = MuSet.make();
	for (int i = 0; i < handler.mapCount(); i ++ ) {
		if (handler.isForgotten(i)) {
			result.store(IntegerPos.make(i));
		}
	}
	releaseReadHandler(handler);
	return result;
/*
udanax-top.st:17462:SnarfPacker methodsFor: 'private:'!
{MuSet of: IntegerPos} forgottenFlocks: snarfID {SnarfID}
	"Return the set of indices to flocks in snarf snarfID that are forgotten."
	
	| result {MuSet of: IntegerPos} handler {SnarfHandler} |
	handler _ self getReadHandler: snarfID.
	result _ MuSet make.
	Int32Zero almostTo: handler mapCount do:
		[:i {Int32} | (handler isForgotten: i) ifTrue: [result store: i integer]].
	self releaseReadHandler: handler.
	^result!
*/
}
/**
 * Return a flock at a particular location.  This needs to register
 * the flock if it doesn't exist already.
 */
public Abraham getFlock(int snarfID, int index) {
	XnReadStream stream;
	Rcvr rcvr;
	Abraham result;
	SnarfHandler handler;
	FlockLocation forward;
	handler = getReadHandler(snarfID);
	/* Follow forwarders. */
	forward = handler.fetchForward(index);
	if (forward != null) {
		return getFlock(forward.snarfID(), forward.index());
	}
	rcvr = makeRcvr((stream = handler.readStream(index)));
	result = (Abraham) rcvr.receiveHeaper();
	rcvr.destroy();
	stream.destroy();
	if (handler.isForgotten(index)) {
		addInfo((FlockInfo.forgotten(result, snarfID, index)), result);
	}
	else {
		addInfo((FlockInfo.remembered(result, snarfID, index)), result);
	}
	result.getInfo().setSize((handler.flockSize(index)));
	releaseReadHandler(handler);
	handler = null;
	return result;
/*
udanax-top.st:17473:SnarfPacker methodsFor: 'private:'!
{Abraham} getFlock: snarfID {SnarfID} with: index {Int32}
	"Return a flock at a particular location.  This needs to register
	 the flock if it doesn't exist already."
	 
	| stream {XnReadStream} rcvr {Rcvr} result {Abraham} handler {SnarfHandler} forward {FlockLocation} |
	handler _ self getReadHandler: snarfID.
	"Follow forwarders."
	forward _ handler fetchForward: index. 
	forward ~~ NULL ifTrue: [^self getFlock: forward snarfID with: forward index].
	rcvr _ self makeRcvr: (stream _ handler readStream: index).
	result _ rcvr receiveHeaper cast: Abraham.
	rcvr destroy.
	stream destroy.
	(handler isForgotten: index)
		ifTrue: [self addInfo: (FlockInfo forgotten: result with: snarfID with: index) with: result]
		ifFalse: [self addInfo: (FlockInfo remembered: result with: snarfID with: index) with: result].
	result getInfo setSize: (handler flockSize: index).
	self releaseReadHandler: handler. 
	handler _ NULL.
	^result!
*/
}
/**
 * Get the read handler on the snarf.
 */
public SnarfHandler getReadHandler(int snarfID) {
	if ( ! ((mySnarfInfo.getSpaceLeft(snarfID)) <= (myUrdiView.getDataSizeOfSnarf(snarfID)))) {
		throw new AboraAssertionException("Handle must aready be initialized");
	}
	return SnarfHandler.make((myUrdiView.makeReadHandle(snarfID)));
/*
udanax-top.st:17494:SnarfPacker methodsFor: 'private:'!
{SnarfHandler} getReadHandler: snarfID {SnarfID}
	"Get the read handler on the snarf."
	
	(mySnarfInfo getSpaceLeft: snarfID) <= (myUrdiView getDataSizeOfSnarf: snarfID)
		assert: 'Handle must aready be initialized'.
	^SnarfHandler make: (myUrdiView makeReadHandle: snarfID)!
*/
}
/**
 * Return the snarfRecord for snarfID.  The SnarfRecord must exist if there are
 * changed flocks imaged out of that snarf, but might not otherwise.  Create it if necessary.
 */
public SnarfRecord getSnarfRecord(int snarfID) {
	SnarfRecord rec;
	rec = (SnarfRecord) (mySnarfMap.intFetch(snarfID));
	if (rec == null) {
		int spaceLeft;
		spaceLeft = mySnarfInfo.getSpaceLeft(snarfID);
		rec = SnarfRecord.make(snarfID, this, spaceLeft);
		mySnarfMap.intIntroduce(snarfID, rec);
	}
	return rec;
/*
udanax-top.st:17501:SnarfPacker methodsFor: 'private:'!
{SnarfRecord} getSnarfRecord: snarfID {SnarfID}
	"Return the snarfRecord for snarfID.  The SnarfRecord must exist if there are
	 changed flocks imaged out of that snarf, but might not otherwise.  Create it if necessary."
	 
	| rec {SnarfRecord} |
	rec _ (mySnarfMap intFetch: snarfID) cast: SnarfRecord.
	rec == NULL ifTrue:
		[| spaceLeft {Int32} | 
		spaceLeft _ mySnarfInfo getSpaceLeft: snarfID.
		rec _ SnarfRecord make: snarfID with: self with: spaceLeft.
		mySnarfMap atInt: snarfID introduce: rec].
	^rec!
*/
}
/**
 * The flock represented by info has changed.  Record it in the
 * bookkeeping data-structures.  This must be called by all things
 * that affect whether the flock gets rewritten to disk.
 */
public void recordUpdate(FlockInfo info) {
	/* The following test should be unnecessary because infos for
	 new flocks should already be dirty, so we shouldn't get here. */
	if ( ! info.isNew()) {
		(getSnarfRecord(info.snarfID())).changedFlock(info.index(), info.getShepherd());
	}
/*
udanax-top.st:17514:SnarfPacker methodsFor: 'private:'!
{void} recordUpdate: info {FlockInfo} 
	"The flock represented by info has changed.  Record it in the
	 bookkeeping data-structures.  This must be called by all things 
	 that affect whether the flock gets rewritten to disk."
	"The following test should be unnecessary because infos for
	 new flocks should already be dirty, so we shouldn't get here."
	info isNew not ifTrue: [(self getSnarfRecord: info snarfID) changedFlock: info index with: info getShepherd]!
*/
}
/**
 * Make sure all flocks that have changed still fit in their snarfs.
 * Add any that don't to myNewFlocks and return the table
 * from their current locations to the newShepherds.
 */
public void refitFlocks() {
	TableStepper stomper = mySnarfMap.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		int snarfID = (int) stomper.index();
		SnarfRecord rec = (SnarfRecord) stomper.fetch();
		if (rec == null) {
			continue ;
		}
		rec.refitFlocks();
		mySnarfInfo.setSpaceLeft(snarfID, rec.spaceLeft());
	}
	stomper.destroy();
/*
udanax-top.st:17523:SnarfPacker methodsFor: 'private:'!
{void} refitFlocks
	"Make sure all flocks that have changed still fit in their snarfs. 
	 Add any that don't to myNewFlocks and return the table 
	 from their current locations to the newShepherds."
	mySnarfMap stepper forIndices: 
		[:snarfID {IntegerVar} :rec {SnarfRecord} |
		rec refitFlocks.
		mySnarfInfo setSpaceLeft: snarfID DOTasLong with: rec spaceLeft]!
*/
}
/**
 * Release the supplied snarfHandler and destroy it.
 */
public void releaseReadHandler(SnarfHandler handler) {
	if ( ! ( ! handler.isWritable())) {
		throw new AboraAssertionException("Must be read handle");
	}
	handler.destroy();
/*
udanax-top.st:17533:SnarfPacker methodsFor: 'private:'!
{void} releaseReadHandler: handler {SnarfHandler}
	"Release the supplied snarfHandler and destroy it."
	handler isWritable not assert: 'Must be read handle'.
	handler destroy!
*/
}
/**
 * Make sure that the shepherd or stub at that location actually points
 * at the real location for a shepherd.  This will resolve forwarding pointers,
 * but not instantiate any flocks.
 */
public FlockInfo resolveLocation(FlockInfo info) {
	FlockInfo newInfo;
	FlockLocation loc;
	SnarfHandler handler;
	if ( ! ( ! info.isNew())) {
		throw new AboraAssertionException("No new flocks allowed");
	}
	loc = null;
	newInfo = info;
	while ((loc = (handler = getReadHandler(newInfo.snarfID())).fetchForward(newInfo.index())) != null) {
		releaseReadHandler(handler);
		newInfo = FlockInfo.make(info, loc.snarfID(), loc.index());
		addInfo(newInfo, info.getShepherd());
	}
	releaseReadHandler(handler);
	return newInfo;
/*
udanax-top.st:17539:SnarfPacker methodsFor: 'private:'!
{FlockInfo} resolveLocation: info {FlockInfo}
	"Make sure that the shepherd or stub at that location actually points
	 at the real location for a shepherd.  This will resolve forwarding pointers, 
	 but not instantiate any flocks."
	| newInfo {FlockInfo} loc {FlockLocation} handler {SnarfHandler} |
	info isNew not assert: 'No new flocks allowed'.
	loc _ NULL.
	newInfo _ info.
	[(loc _ (handler _ self getReadHandler: newInfo snarfID) fetchForward: newInfo index) ~~ NULL] whileTrue: 
		[self releaseReadHandler: handler.
		newInfo _ FlockInfo make: info with: loc snarfID with: loc index.
		self addInfo: newInfo with: info getShepherd].
	self releaseReadHandler: handler.
	^newInfo!
*/
}
public SnarfPacker(Urdi urdi) {
	super();
	myTurtle = null;
	myXcvrMaker = XcvrMaker.make();
	/* Put in a bogus protocol maker. */
	myBook = null;
	myUrdi = urdi;
	myUrdiView = urdi.makeWriteView();
	mySnarfInfo = SnarfInfoHandler.make(urdi, myUrdiView);
	myAllocationSnarf = 0;
	mySnarfMap = IntegerTable.make(50);
	myFlocks = SetTable.make(IntegerSpace.make(), 501);
	myNewFlocks = IntegerTable.make(500);
	myDestroyedFlocks = MuArray.array();
	myConsistentCount = 0;
	myNextHash = null;
	myInsideCommit = false;
	myDestroyCount = 0;
	myPurgeror = Purgeror.make(this);
	myRepairer = LiberalPurgeror.make(this);
	myNewEstimate = 0;
	myLastNewCount = 0;
	PersistentCleaner.make();
	/* AbandonDisk make: self. */
/*
udanax-top.st:17557:SnarfPacker methodsFor: 'protected: creation'!
create: urdi {Urdi}
	super create.
	myTurtle _ NULL.
	myXcvrMaker _ XcvrMaker make.  "Put in a bogus protocol maker."
	myBook _ NULL.
	myUrdi _ urdi.
	myUrdiView _ urdi makeWriteView.
	mySnarfInfo _ SnarfInfoHandler make: urdi with: myUrdiView.
	myAllocationSnarf _ Int32Zero.
	mySnarfMap _ IntegerTable make: 50.
	myFlocks _ SetTable make: IntegerSpace make with: 501.
	myNewFlocks _ IntegerTable make: 500.
	myDestroyedFlocks _ MuArray array.
	myConsistentCount _ IntegerVarZero.
	myNextHash _ NULL.
	myInsideCommit _ false.
	myDestroyCount _ Int32Zero.
	myPurgeror _ Purgeror make: self.
	myRepairer _ LiberalPurgeror make: self.
	myNewEstimate _ IntegerVarZero.
	myLastNewCount _ IntegerVarZero.
	PersistentCleaner make.
	"AbandonDisk make: self."!
*/
}
public int consistentCount() {
	return myConsistentCount;
/*
udanax-top.st:17583:SnarfPacker methodsFor: 'smalltalk: testing'!
consistentCount
	^myConsistentCount!
*/
}
public void purgeClean() {
	purgeClean(false);
/*
udanax-top.st:17588:SnarfPacker methodsFor: 'smalltalk: defaults'!
{void} purgeClean
	self purgeClean: false!
*/
}
/**
 * @deprecated
 */
public void consistent(int dirty, BlockClosure aBlock) {
	throw new PasseException();
/*
udanax-top.st:17593:SnarfPacker methodsFor: 'smalltalk: passe'!
{void} consistent: dirty {IntegerVar} with: aBlock {BlockClosure}
	self passe.
	myInsideCommit not assert: 'Transaction are outside commit operations'.
	InsideTransactionFlag fluidFetch
		ifTrue: [aBlock value]
		ifFalse: 
			[self makeConsistentBegin: dirty.
			InsideTransactionFlag fluidBind: true during: aBlock.
			self makeConsistentEnd]!
*/
}
public boolean isFake() {
	return false;
/*
udanax-top.st:17606:SnarfPacker methodsFor: 'testing'!
{BooleanVar} isFake
	^ false!
*/
}
public static void linkTimeNonInherited() {
	LRUCount = 50;
/*
udanax-top.st:17621:SnarfPacker class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	LRUCount _ 50.!
*/
}
public static DiskManager initializeUrdiOnDisk(String fname) {
	Urdi anUrdi;
	UrdiView view;
	DiskManager disk;
	anUrdi = Urdi.urdi(fname, LRUCount);
	view = anUrdi.makeWriteView();
	SnarfInfoHandler.initializeSnarfInfo(anUrdi, view);
	view.commitWrite();
	view.destroy();
	disk = new SnarfPacker(anUrdi);
	CurrentPacker.fluidSet(disk);
	return ((DiskManager) CurrentPacker.fluidGet());
/*
udanax-top.st:17626:SnarfPacker class methodsFor: 'creation'!
{DiskManager} initializeUrdiOnDisk: fname {char star}
	
	| anUrdi {Urdi} view {UrdiView} disk {DiskManager} |
	anUrdi _ Urdi urdi: fname with: LRUCount.
	view _ anUrdi makeWriteView.
	SnarfInfoHandler initializeSnarfInfo: anUrdi with: view.
	view commitWrite.
	view destroy.
	disk _ SnarfPacker create: anUrdi.
	CurrentPacker fluidSet: disk. 
	^CurrentPacker fluidGet!
*/
}
public static DiskManager make(String fname) {
	return new SnarfPacker((Urdi.urdi(fname, LRUCount)));
/*
udanax-top.st:17638:SnarfPacker class methodsFor: 'creation'!
make: fname {char star}
	
	^self create: (Urdi urdi: fname with: LRUCount)!
*/
}
public static void bombResetCommit(SnarfPacker CHARGE) {
	CHARGE.commitState(false);
/*
udanax-top.st:17644:SnarfPacker class methodsFor: 'exceptions: private:'!
bomb.ResetCommit: CHARGE {SnarfPacker}
	^[CHARGE commitState: false]!
*/
}
public SnarfPacker() {
/*

Generated during transformation
*/
}
public SnarfPacker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
