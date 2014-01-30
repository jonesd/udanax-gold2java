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
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.Pumpkin;
import info.dgjones.abora.gold.snarf.TestFlockInfo;
import info.dgjones.abora.gold.snarf.TestPacker;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.stacker.StackExaminer;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.urdi.CountStream;
import info.dgjones.abora.gold.urdi.HashStream;
import info.dgjones.abora.gold.xcvr.DiskSpecialist;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Does not actually go to disk, but just tests that the protocol is being followed
 * correctly. Some of these tests may make it into the real SnarfPacker, but some of them
 * will remain debugging tools. Most operations only do enough real stuff to be able to check
 * that they work.
 * The TestPacker holds onto an IntegerTable of UInt8Arrays that contain the disk
 * representations of all the flocks.  It also holds
 * myDisk contains a UInt8Array for every flock that made it to disk.  They are assigned
 * sequential numbers.
 * myNewFlocks contains the flockInfos for new flocks, and thus contains the new flocks
 * wimpily.
 * myAlmostNewFlocks contains flocks that are under construction but have not yet finished.
 * myDestroyedFlocks contains flocks that will be destroyed upon exiting the current
 * consistent block.
 * myChangedFlocks points strongly at flocks that must be rewritten to disk.
 */
public class TestPacker extends DiskManager {

	protected int myNextHash;
	protected Abraham myInitialFlock;
	protected IntegerTable myFlocks;
	protected IntegerTable myChangedFlocks;
	protected IntegerTable myDestroyedFlocks;
	protected MuSet myAlmostNewFlocks;
	protected IntegerTable myNewFlocks;
	protected XcvrMaker myXcvrMaker;
	protected int myCountDown;
	protected int myPersistInterval;
	protected IntegerTable myDisk;
	protected Cookbook myBook;
	protected boolean amCommitting;
	protected boolean blastOnError;
/*
udanax-top.st:17648:
DiskManager subclass: #TestPacker
	instanceVariableNames: '
		myNextHash {UInt32}
		myInitialFlock {Abraham}
		myFlocks {IntegerTable of: FlockInfo}
		myChangedFlocks {IntegerTable of: Abraham}
		myDestroyedFlocks {IntegerTable of: Abraham}
		myAlmostNewFlocks {MuSet of: Abraham}
		myNewFlocks {IntegerTable of: FlockInfo}
		myXcvrMaker {XcvrMaker}
		myCountDown {IntegerVar}
		myPersistInterval {IntegerVar}
		myDisk {IntegerTable of: UInt8Array}
		myBook {Cookbook}
		amCommitting {BooleanVar}
		blastOnError {BooleanVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:17666:
TestPacker comment:
'Does not actually go to disk, but just tests that the protocol is being followed correctly. Some of these tests may make it into the real SnarfPacker, but some of them will remain debugging tools. Most operations only do enough real stuff to be able to check that they work.
The TestPacker holds onto an IntegerTable of UInt8Arrays that contain the disk representations of all the flocks.  It also holds 
myDisk contains a UInt8Array for every flock that made it to disk.  They are assigned sequential numbers.
myNewFlocks contains the flockInfos for new flocks, and thus contains the new flocks wimpily.
myAlmostNewFlocks contains flocks that are under construction but have not yet finished.
myDestroyedFlocks contains flocks that will be destroyed upon exiting the current consistent block.
myChangedFlocks points strongly at flocks that must be rewritten to disk.
'!
*/
/*
udanax-top.st:17678:
(TestPacker getOrMakeCxxClassDescription)
	friends:
'friend class EndCommit_Bomb;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:18117:
TestPacker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:18120:
(TestPacker getOrMakeCxxClassDescription)
	friends:
'friend class EndCommit_Bomb;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TestPacker.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Queue destroy of the given flock.  The destroy will probably happen later.
 */
public void destroyFlock(FlockInfo info) {
	Abraham flock;
	flock = (Abraham) info.getShepherd();
	/* Check for destructed essentially */
	mustKnowShepherd(info);
	mustBeInsideTransaction();
	mustNotBeCommitting();
	countDown();
	info.markDestroyed();
	if (info.markForgotten()) {
		recordUpdate(info);
	}
	myDestroyedFlocks.intIntroduce(myDestroyedFlocks.count(), flock);
/*
udanax-top.st:17685:TestPacker methodsFor: 'shepherds'!
{void} destroyFlock: info {FlockInfo} 
	"Queue destroy of the given flock.  The destroy will probably happen later."
	| flock {Abraham} |
	flock _ info getShepherd cast: Abraham. "Check for destructed essentially"
	self mustKnowShepherd: info.
	self mustBeInsideTransaction.
	self mustNotBeCommitting.
	self countDown.
	
	info markDestroyed.
	info markForgotten ifTrue: [self recordUpdate: info].
	myDestroyedFlocks atInt: myDestroyedFlocks count introduce: flock!
*/
}
public void diskUpdate(FlockInfo info) {
	if (info == null) {
		return ;
	}
	/* noop for new shepherds. */
	mustKnowShepherd(info);
	mustBeInsideTransaction();
	mustNotBeCommitting();
	countDown();
	if (info.markContentsDirty()) {
		recordUpdate(info);
	}
	else {
		/* sanity check */
		if (info.isNew()) {
			if ( ! (myNewFlocks.includesIntKey(info.index()))) {
				throw new AboraAssertionException("Something is wrong");
			}
		}
		else {
			if ( ! (myChangedFlocks.includesIntKey(info.index()))) {
				throw new AboraAssertionException("Something is wrong");
			}
		}
	}
/*
udanax-top.st:17699:TestPacker methodsFor: 'shepherds'!
{void} diskUpdate: info {FlockInfo} 
	info == NULL ifTrue: [^VOID].  "noop for new shepherds."
	self mustKnowShepherd: info.
	self mustBeInsideTransaction.
	self mustNotBeCommitting.
	self countDown.
	info markContentsDirty
		ifTrue: [self recordUpdate: info]
		ifFalse: 
			["sanity check"
			info isNew
				ifTrue: [(myNewFlocks includesIntKey: info index) assert: 'Something is wrong']
				ifFalse: [(myChangedFlocks includesIntKey: info index) assert: 'Something is wrong']]!
*/
}
/**
 * The flock designated by info has completed all dismantling actions; throw it off the disk.
 */
public void dismantleFlock(FlockInfo info) {
	Abraham flock;
	flock = (Abraham) info.getShepherd();
	/* Check for destructed essentially */
	mustKnowShepherd(info);
	mustNotBeCommitting();
	countDown();
	info.markDismantled();
	if ( ! (info.isNew())) {
		myChangedFlocks.intStore(info.index(), Pumpkin.make());
	}
/*
udanax-top.st:17713:TestPacker methodsFor: 'shepherds'!
{void} dismantleFlock: info {FlockInfo} 
	"The flock designated by info has completed all dismantling actions; throw it off the disk."
	| flock {Abraham} |
	flock _ info getShepherd cast: Abraham. "Check for destructed essentially"
	self mustKnowShepherd: info.
	self mustNotBeCommitting.
	self countDown.
	
	info markDismantled.
	info isNew ifFalse:
		[myChangedFlocks atInt: info index store: Pumpkin make].!
*/
}
public void dropFlock(int token) {
	FlockInfo info;
	info = FlockInfo.getInfo(token);
	if (info.isNew()) {
		myNewFlocks.intRemove(info.index());
	}
	else {
		if ( ! (info.isForgotten())) {
			throw new AboraRuntimeException(AboraRuntimeException.ONLY_REMOVE_UNCHANGED_FLOCKS);
		}
		myChangedFlocks.intWipe(info.index());
		myFlocks.intRemove(info.index());
	}
	FlockInfo.removeInfo(token);
/*
udanax-top.st:17726:TestPacker methodsFor: 'shepherds'!
{void} dropFlock: token {Int32}
	| info {FlockInfo} |
	info := FlockInfo getInfo: token.
	info isNew
		ifTrue: 
			[myNewFlocks intRemove: info index]
		ifFalse: 
			[info isForgotten ifFalse: [Heaper BLAST: #OnlyRemoveUnchangedFlocks].
			myChangedFlocks intWipe: info index.
			myFlocks intRemove: info index].
	FlockInfo removeInfo: token!
*/
}
public void forgetFlock(FlockInfo info) {
	mustKnowShepherd(info);
	mustBeInsideTransaction();
	mustNotBeCommitting();
	countDown();
	if (info.markForgotten()) {
		recordUpdate(info);
	}
/*
udanax-top.st:17738:TestPacker methodsFor: 'shepherds'!
{void} forgetFlock: info {FlockInfo} 
	self mustKnowShepherd: info.
	self mustBeInsideTransaction.
	self mustNotBeCommitting.
	self countDown.
	info markForgotten ifTrue: [self recordUpdate: info]!
*/
}
public Turtle getInitialFlock() {
	return (Turtle) myInitialFlock;
/*
udanax-top.st:17746:TestPacker methodsFor: 'shepherds'!
{Turtle} getInitialFlock
	^myInitialFlock cast: Turtle!
*/
}
public int nextHashForEqual() {
	myNextHash = myNextHash + 1;
	/* This actually needs to roll over the UInt32 limit. */
	return myNextHash;
/*
udanax-top.st:17750:TestPacker methodsFor: 'shepherds'!
{UInt32} nextHashForEqual
	 
	myNextHash _ myNextHash + 1.
	"This actually needs to roll over the UInt32 limit."
	^myNextHash!
*/
}
public void rememberFlock(FlockInfo info) {
	mustBeInsideTransaction();
	countDown();
	if (info.markRemembered()) {
		recordUpdate(info);
	}
/*
udanax-top.st:17756:TestPacker methodsFor: 'shepherds'!
{void} rememberFlock: info {FlockInfo} 
	self mustBeInsideTransaction.
	self countDown.
	info markRemembered ifTrue: [self recordUpdate: info]!
*/
}
public void storeAlmostNewShepherd(Abraham shep) {
	myAlmostNewFlocks.store(shep);
/*
udanax-top.st:17762:TestPacker methodsFor: 'shepherds'!
{void} storeAlmostNewShepherd: shep {Abraham} 
	myAlmostNewFlocks store: shep!
*/
}
public void storeInitialFlock(Abraham turtle, XcvrMaker protocol, Cookbook cookbook) {
	myInitialFlock = turtle;
	myXcvrMaker = protocol;
	myBook = cookbook;
	storeNewFlock(turtle);
/*
udanax-top.st:17766:TestPacker methodsFor: 'shepherds'!
{void} storeInitialFlock: turtle {Abraham}
	with: protocol {XcvrMaker}
	with: cookbook {Cookbook}
	
	myInitialFlock := turtle.
	myXcvrMaker := protocol.
	myBook := cookbook.
	self storeNewFlock: turtle.!
*/
}
/**
 * Shep just got created!! On some later commit, assign it to a snarf
 * and write it to the disk.
 */
public void storeNewFlock(Abraham shep) {
	FlockInfo info;
	if ( ! (shep.fetchInfo() == null)) {
		throw new AboraRuntimeException(AboraRuntimeException.NEW_SHEPHERD_MUST_NOT_HAVE_INFO);
	}
	countDown();
	myAlmostNewFlocks.wipe(shep);
	info = TestFlockInfo.make(shep, myNewFlocks.highestIndex() + 1);
	myNewFlocks.intIntroduce(myNewFlocks.highestIndex() + 1, info);
	shep.flockInfo(info);
/*
udanax-top.st:17775:TestPacker methodsFor: 'shepherds'!
{void} storeNewFlock: shep {Abraham} 
	"Shep just got created!! On some later commit, assign it to a snarf 
	and write it to the disk."
	| info {FlockInfo} |
	shep fetchInfo == NULL ifFalse:
		[Heaper BLAST: #NewShepherdMustNotHaveInfo].
	self countDown.
	myAlmostNewFlocks wipe: shep.
	info _ TestFlockInfo make: shep with: myNewFlocks highestIndex + 1.
	myNewFlocks atInt: myNewFlocks highestIndex + 1 introduce: info.
	shep flockInfo: info!
*/
}
public void checkNewFlockIndices() {
	TableStepper stomper = myNewFlocks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		int index = (int) stomper.index();
		FlockInfo value = (FlockInfo) stomper.fetch();
		if (value == null) {
			continue ;
		}
		if ( ! (index == value.index())) {
			throw new AboraRuntimeException(AboraRuntimeException.NEW_FLOCK_INDEX_DOES_NOT_MATCH);
		}
	}
	stomper.destroy();
/*
udanax-top.st:17790:TestPacker methodsFor: 'private: testing'!
{void} checkNewFlockIndices
	myNewFlocks stepper forIndices: [ :index {IntegerVar} :value {FlockInfo} |
		index DOTasLong = value index ifFalse:
			[Heaper BLAST: #NewFlockIndexDoesNotMatch]]!
*/
}
public void committing(boolean flag) {
	amCommitting = flag;
/*
udanax-top.st:17796:TestPacker methodsFor: 'private: testing'!
{void} committing: flag {BooleanVar}
	amCommitting := flag!
*/
}
/**
 * Decrement the countdown and return its new value
 */
public int countDown() {
	myCountDown = myCountDown - 1;
	return myCountDown;
/*
udanax-top.st:17800:TestPacker methodsFor: 'private: testing'!
{IntegerVar} countDown
	"Decrement the countdown and return its new value"
	
	myCountDown := myCountDown - 1.
	^myCountDown!
*/
}
public void mustBeInsideTransaction() {
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		if (blastOnError) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_INSIDE_TRANSACTION);
		}
		/* Removed smalltalkOnly */
		AboraSupport.logger.print("A consistent block is missing\n"+
"");
	}
/*
udanax-top.st:17806:TestPacker methodsFor: 'private: testing'!
{void} mustBeInsideTransaction
	InsideTransactionFlag fluidFetch
		ifFalse: [blastOnError ifTrue:
				[Heaper BLAST: #MustBeInsideTransaction].
			[cerr << 'Method '<< thisContext sender sender selector << ' must call ' << thisContext sender selector << ' inside a transaction
'] smalltalkOnly.
			cerr << 'A consistent block is missing
']!
*/
}
/**
 * Check that I know about this shepherd
 */
public void mustKnowShepherd(FlockInfo info) {
	Heaper t;
	if (info.isNew()) {
		t = myNewFlocks.intFetch(info.index());
	}
	else {
		t = myFlocks.intFetch(info.index());
	}
	if ( ! (t != null && (t.isEqual(info)))) {
		throw new AboraRuntimeException(AboraRuntimeException.INCORRECT_FLOCK_INFO);
	}
/*
udanax-top.st:17816:TestPacker methodsFor: 'private: testing'!
{void} mustKnowShepherd: info {FlockInfo} 
	"Check that I know about this shepherd"
	| t {Heaper} |
	info isNew
		ifTrue: [t := myNewFlocks intFetch: info index]
		ifFalse: [t := myFlocks intFetch: info index].
	(t ~~ NULL and: [t isEqual: info])
		ifFalse: [Heaper BLAST: #IncorrectFlockInfo]!
*/
}
public void mustNotBeCommitting() {
	if (amCommitting) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_NOT_CHANGE_DURING_COMMIT);
	}
/*
udanax-top.st:17826:TestPacker methodsFor: 'private: testing'!
{void} mustNotBeCommitting
	amCommitting ifTrue:
		[Heaper BLAST: #MustNotChangeDuringCommit]!
*/
}
public void resetCountDown() {
	myCountDown = myPersistInterval;
/*
udanax-top.st:17831:TestPacker methodsFor: 'private: testing'!
{void} resetCountDown
	myCountDown := myPersistInterval.!
*/
}
public Abraham fetchCanonical(int hash, int snarfID, int index) {
	return (Abraham) (myFlocks.intFetch(index));
/*
udanax-top.st:17837:TestPacker methodsFor: 'stubs'!
{Abraham} fetchCanonical: hash {UInt32 unused} with: snarfID {SnarfID unused} with: index {Int32}
	^(myFlocks intFetch: index) cast: Abraham!
*/
}
public void makeReal(FlockInfo info) {
	Abraham stub;
	int oldHash;
	XnReadStream stream;
	Rcvr rcvr;
	stub = info.getShepherd();
	if ( ! (stub.isStub())) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_ASTUB);
	}
	oldHash = stub.hashForEqual();
	(rcvr = makeRcvr((stream = readStream(info)))).receiveInto(stub);
	rcvr.destroy();
	stream.destroy();
	if ( ! (stub.hashForEqual() == oldHash)) {
		throw new AboraRuntimeException(AboraRuntimeException.HASH_MUST_NOT_CHANGE);
	}
	info.setSize((computeSize(info.getShepherd())));
	/* Receiving the flock will have cleared its info, so put it back. */
	stub.flockInfo(info);
/*
udanax-top.st:17841:TestPacker methodsFor: 'stubs'!
{void} makeReal: info {FlockInfo}
	| stub {Abraham} oldHash {UInt32} stream {XnReadStream} rcvr {Rcvr} |
	stub := info getShepherd.
	stub isStub ifFalse:
		[Heaper BLAST: #MustBeAStub].
	oldHash := stub hashForEqual.
	(rcvr _ self makeRcvr: (stream _ self readStream: info)) receiveInto: stub.
	rcvr destroy.
	stream destroy.
	stub hashForEqual == oldHash
		ifFalse: [Heaper BLAST: #HashMustNotChange].
	info setSize: (self computeSize: info getShepherd).
	"Receiving the flock will have cleared its info, so put it back."
	stub flockInfo: info!
*/
}
public void registerStub(Abraham shep, int snarfID, int index) {
	FlockInfo info;
	if ( ! (shep.isStub())) {
		throw new AboraAssertionException("Must be stub");
	}
	info = TestFlockInfo.remembered(shep, snarfID, index);
	shep.flockInfo(info);
	myFlocks.intIntroduce(index, info);
/*
udanax-top.st:17857:TestPacker methodsFor: 'stubs'!
{void} registerStub: shep {Abraham} with: snarfID {SnarfID} with: index {Int32}
	| info {FlockInfo} |
	shep isStub assert: 'Must be stub'.
	info _ TestFlockInfo remembered: shep with: snarfID with: index.
	shep flockInfo: info.
	myFlocks atInt: index introduce: info!
*/
}
/**
 * Send the snarf over a transmitter into a stream that just counts the bytes put into it.
 */
public int computeSize(Abraham flock) {
	XnWriteStream counter;
	Xmtr xmtr;
	int size;
	counter = CountStream.make();
	xmtr = makeXmtr(counter);
	xmtr.sendHeaper(flock);
	size = ((CountStream) counter).size();
	xmtr.destroy();
	counter.destroy();
	return size;
/*
udanax-top.st:17866:TestPacker methodsFor: 'private: streams'!
{Int32} computeSize: flock {Abraham}
	"Send the snarf over a transmitter into a stream that just counts the bytes put into it."
	
	| counter {XnWriteStream} xmtr {Xmtr} size {Int32} | 
	counter := CountStream make.
	xmtr _ self makeXmtr: counter.
	xmtr sendHeaper: flock.
	size _ (counter cast: CountStream) size.
	xmtr destroy.
	counter destroy.
	^size!
*/
}
public SpecialistRcvr makeRcvr(XnReadStream readStream) {
	return myXcvrMaker.makeRcvr((DiskSpecialist.make(myBook, this)), readStream);
/*
udanax-top.st:17878:TestPacker methodsFor: 'private: streams'!
{SpecialistRcvr} makeRcvr: readStream {XnReadStream}
	^myXcvrMaker makeRcvr: (DiskSpecialist make: myBook with: self) with: readStream!
*/
}
public SpecialistXmtr makeXmtr(XnWriteStream writeStream) {
	return myXcvrMaker.makeXmtr((DiskSpecialist.make(myBook, this)), writeStream);
/*
udanax-top.st:17882:TestPacker methodsFor: 'private: streams'!
{SpecialistXmtr} makeXmtr: writeStream {XnWriteStream}
	^myXcvrMaker makeXmtr: (DiskSpecialist make: myBook with: self) with: writeStream!
*/
}
/**
 * Get a read stream on the disk contents of the info
 */
public XnReadStream readStream(FlockInfo info) {
	return XnReadStream.make(((UInt8Array) (myDisk.intGet(info.index()))));
/*
udanax-top.st:17886:TestPacker methodsFor: 'private: streams'!
{XnReadStream} readStream: info {FlockInfo}
	"Get a read stream on the disk contents of the info"
	
	^XnReadStream make: ((myDisk intGet: info index) cast: UInt8Array)!
*/
}
/**
 * Get a write stream on the disk contents of the info
 */
public XnWriteStream writeStream(FlockInfo info) {
	UInt8Array result;
	result = UInt8Array.make((computeSize(info.getShepherd())));
	myDisk.intStore(info.index(), result);
	Someone.hack();
	/* You can't use gutsOf in something that will do an allocation. */
	return XnWriteStream.make(result);
/*
udanax-top.st:17891:TestPacker methodsFor: 'private: streams'!
{XnWriteStream} writeStream: info {FlockInfo}
	"Get a write stream on the disk contents of the info"
	
	| result {UInt8Array} |
	result := UInt8Array make: (self computeSize: info getShepherd).
	myDisk atInt: info index store: result.
	self hack.  "You can't use gutsOf in something that will do an allocation."
	^XnWriteStream make: result!
*/
}
public void assignSnarf(Abraham shep) {
	FlockInfo oldInfo;
	int snarf;
	oldInfo = shep.getInfo();
	snarf = myDisk.highestIndex() + 1;
	myDisk.intStore(snarf, (UInt8Array.make(0)));
	shep.flockInfo((TestFlockInfo.make(oldInfo, snarf, snarf)));
	/* Destroy the old location if it is for a new flock (rather than forwarded). */
	if (oldInfo.isNew()) {
		myNewFlocks.intWipe(oldInfo.index());
		oldInfo.destroy();
		((TestFlockInfo) shep.getInfo()).updateContentsInfo();
	}
	oldInfo = null;
	myFlocks.intIntroduce(snarf, shep.getInfo());
	myChangedFlocks.intStore(snarf, shep);
/*
udanax-top.st:17902:TestPacker methodsFor: 'private: disk'!
{void} assignSnarf: shep {Abraham}
	| oldInfo {FlockInfo} snarf {SnarfID} |
	oldInfo := shep getInfo.
	snarf := myDisk highestIndex DOTasLong + 1.
	myDisk atInt: snarf store: (UInt8Array make: UInt32Zero).
	shep flockInfo: (TestFlockInfo make: oldInfo with: snarf with: snarf).
	"Destroy the old location if it is for a new flock (rather than forwarded)."
	oldInfo isNew ifTrue:
		[myNewFlocks intWipe: oldInfo index.
		oldInfo destroy.
		(shep getInfo cast: TestFlockInfo) updateContentsInfo].
	oldInfo := NULL.
	myFlocks atInt: snarf introduce: shep getInfo.
	myChangedFlocks atInt: snarf store: shep!
*/
}
/**
 * Rewrite all flocks that have changed in this snarf.
 */
public void flushChanges() {
	/* check that all changed flocks are in fact in myChangedFlocks */
	TableStepper flocks;
	Stepper stomper = myFlocks.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		TestFlockInfo info = (TestFlockInfo) stomper.fetch();
		if (info == null) {
			continue ;
		}
		if (info.fetchShepherd() != null && (( ! info.isNew()) && ((info.updateContentsInfo() || (info.isContentsDirty())) && ( ! (myChangedFlocks.includesIntKey(info.snarfID())))))) {
			if (blastOnError) {
				throw new AboraRuntimeException(AboraRuntimeException.SHOULD_HAVE_DONE_DISK_UPDATE_ON_CHANGED_SHEPHERD);
			}
			AboraSupport.logger.print("Shepherd ");
			AboraSupport.logger.print(info.fetchShepherd());
			AboraSupport.logger.print(" with info ");
			AboraSupport.logger.print(info);
			AboraSupport.logger.print(" should have done a diskUpdate\n"+
"");
			recordUpdate(info);
		}
	}
	stomper.destroy();
	Stepper stomper2 = 
	/* actually write changed flocks to disk */
	(flocks = myChangedFlocks.stepper());
	for (; stomper2.hasValue(); stomper2.step()) {
		Heaper thing = (Heaper) stomper2.fetch();
		if (thing == null) {
			continue ;
		}
		if (thing instanceof Pumpkin) {
			Pumpkin pumpkin = (Pumpkin) thing;
			myDisk.intWipe(flocks.index());
		}
		else if (thing instanceof Abraham) {
			Abraham shep = (Abraham) thing;
			FlockInfo inf;
			inf = shep.fetchInfo();
			if (inf == null) {
				throw new AboraRuntimeException(AboraRuntimeException.SHEPHERD_MUST_NOT_HAVE_NULL_FLOCK_INFO);
			}
			if (inf.index() == flocks.index()) {
				Xmtr xmtr;
				XnWriteStream stream;
				/* Not forwarded. */
				if (shep.isStub()) {
					throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_INSTANTIATED);
				}
				(xmtr = makeXmtr((stream = writeStream(inf)))).sendHeaper(shep);
				xmtr.destroy();
				stream.destroy();
				((TestFlockInfo) inf).setContents(((UInt8Array) (myDisk.intFetch(inf.index()))));
				inf.commitFlags();
			}
			else {
				/* We only get here for forwarded flocks. */
				throw new AboraRuntimeException(AboraRuntimeException.TEST_PACKER_DOES_NOT_FORWARD);
			}
		}
	}
	stomper2.destroy();
	myChangedFlocks.destroy();
	myChangedFlocks = IntegerTable.make();
/*
udanax-top.st:17918:TestPacker methodsFor: 'private: disk'!
{void} flushChanges
	"Rewrite all flocks that have changed in this snarf."
	"check that all changed flocks are in fact in myChangedFlocks"
	| flocks {TableStepper} |
	myFlocks stepper forEach: [:info {TestFlockInfo} | 
		(info fetchShepherd ~~ NULL
			and: [(info isNew not)
			and: [(info updateContentsInfo or: [info isContentsDirty])
			and: [(myChangedFlocks includesIntKey: info snarfID) not]]])
			ifTrue: 
				[blastOnError ifTrue: [Heaper BLAST: #ShouldHaveDoneDiskUpdateOnChangedShepherd].
				cerr << 'Shepherd ' << info fetchShepherd << ' with info ' << info << ' should have done a diskUpdate
'.
				self recordUpdate: info]].
	"actually write changed flocks to disk"
	(flocks := myChangedFlocks stepper) forEach: [:thing {Heaper} | thing
		cast: Pumpkin into: [:pumpkin |
			myDisk intWipe: flocks index]
		cast: Abraham into: [:shep | | inf {FlockInfo} |
			inf := shep fetchInfo.
			inf == NULL ifTrue: [Heaper BLAST: #ShepherdMustNotHaveNullFlockInfo].
			inf index == flocks index DOTasLong ifTrue: 
				[| xmtr {Xmtr} stream {XnWriteStream} |
				"Not forwarded."
				shep isStub ifTrue: [Heaper BLAST: #MustBeInstantiated].
				(xmtr _ self makeXmtr: (stream _ self writeStream: inf)) sendHeaper: shep.
				xmtr destroy.
				stream destroy.
				(inf cast: TestFlockInfo) setContents: ((myDisk intFetch: inf index) cast: UInt8Array).
				inf commitFlags]
			ifFalse:
				["We only get here for forwarded flocks."
				Heaper BLAST: #TestPackerDoesNotForward]]].
	myChangedFlocks destroy.
	myChangedFlocks := IntegerTable make!
*/
}
/**
 * The flock represented by info has changed.  Record it in the
 * bookkeeping data-structures.  This must be called by all things
 * that affect whether the flock gets rewritten to disk.
 */
public void recordUpdate(FlockInfo info) {
	Abraham shep;
	if ( ! info.isNew()) {
		if ((shep = info.fetchShepherd()) != null) {
			if (shep.isEqual(Pumpkin.make())) {
				if (blastOnError) {
					throw new AboraRuntimeException(AboraRuntimeException.MUST_NOT_RECORD_CHANGES_FOR_PUMPKINS);
				}
				AboraSupport.logger.print("Pumpkin ");
				AboraSupport.logger.print(info);
				AboraSupport.logger.print(" tried to diskUpdate\n"+
"");
				return ;
			}
		}
		myChangedFlocks.intStore(info.index(), shep);
	}
/*
udanax-top.st:17955:TestPacker methodsFor: 'private: disk'!
{void} recordUpdate: info {FlockInfo} 
	"The flock represented by info has changed.  Record it in the
	 bookkeeping data-structures.  This must be called by all things 
	 that affect whether the flock gets rewritten to disk."
	| shep {Abraham} |
	
	info isNew not ifTrue: 
		[(shep _ info fetchShepherd) ~~ NULL ifTrue:
			[(shep isEqual: Pumpkin make)
				ifTrue: 
					[blastOnError ifTrue: [Heaper BLAST: #MustNotRecordChangesForPumpkins].
					cerr << 'Pumpkin ' << info << ' tried to diskUpdate
'.
					^VOID]].
		myChangedFlocks atInt: info index store: shep]!
*/
}
/**
 * do nothing for now
 */
public void refitFlocks() {
/*
udanax-top.st:17971:TestPacker methodsFor: 'private: disk'!
{void} refitFlocks
	"do nothing for now"!
*/
}
public TestPacker(boolean blast, int persistInterval) {
	super();
	myNextHash = 0;
	myInitialFlock = null;
	myFlocks = IntegerTable.make();
	myChangedFlocks = IntegerTable.make();
	myDestroyedFlocks = MuArray.array();
	myAlmostNewFlocks = MuSet.make();
	myNewFlocks = IntegerTable.make();
	myXcvrMaker = null;
	myBook = null;
	myPersistInterval = persistInterval;
	resetCountDown();
	myDisk = IntegerTable.make();
	amCommitting = false;
	blastOnError = blast;
/*
udanax-top.st:17977:TestPacker methodsFor: 'create'!
create: blast {BooleanVar} with: persistInterval {IntegerVar}
	super create.
	myNextHash := UInt32Zero.
	myInitialFlock := NULL.
	myFlocks := IntegerTable make.
	myChangedFlocks := IntegerTable make.
	myDestroyedFlocks _ MuArray array.
	myAlmostNewFlocks := MuSet make.
	myNewFlocks := IntegerTable make.
	myXcvrMaker := NULL.
	myBook := NULL.
	myPersistInterval := persistInterval.
	self resetCountDown.
	myDisk := IntegerTable make.
	amCommitting := false.
	blastOnError := blast.!
*/
}
/**
 * Compute a hash on the contents
 */
public int computeHash(Abraham flock) {
	XnWriteStream hasher;
	int hash;
	SpecialistXmtr xmtr;
	hasher = HashStream.make();
	xmtr = makeXmtr(hasher);
	xmtr.sendHeaper(flock);
	hash = ((HashStream) hasher).hash();
	xmtr.destroy();
	hasher.destroy();
	return hash;
/*
udanax-top.st:17997:TestPacker methodsFor: 'internals'!
{UInt32} computeHash: flock {Abraham}
	"Compute a hash on the contents"
	| hasher {XnWriteStream} hash {UInt32} xmtr {SpecialistXmtr} |
	hasher := HashStream make.
	xmtr := self makeXmtr: hasher.
	xmtr sendHeaper: flock.
	hash := (hasher cast: HashStream) hash.
	xmtr destroy.
	hasher destroy.
	^hash!
*/
}
public void purgeClean() {
	purgeClean(false);
/*
udanax-top.st:18010:TestPacker methodsFor: 'smalltalk: defaults'!
{void} purgeClean
	self purgeClean: false!
*/
}
public void beginConsistent(int dirty) {
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		if (countDown() < 0) {
			makePersistent();
			resetCountDown();
		}
	}
/*
udanax-top.st:18015:TestPacker methodsFor: 'transactions'!
{void} beginConsistent: dirty {IntegerVar unused}
	InsideTransactionFlag fluidFetch
		ifFalse: 
			[self countDown < IntegerVar0
				ifTrue: 
					[self makePersistent.
					self resetCountDown]]!
*/
}
public void endConsistent(int dirty) {
	Agenda agenda;
	if (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue()) {
		return ;
	}
	if ( ! (myAlmostNewFlocks.isEmpty())) {
		if (blastOnError) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_DO_NEW_SHEPHERD_AFTER_DISK_UPDATE);
		}
		AboraSupport.logger.print("These flocks should have done a newShepherd: ");
		AboraSupport.logger.print(myAlmostNewFlocks);
		AboraSupport.logger.print("\n"+
"");
		Stepper stomper = myAlmostNewFlocks.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			Abraham each = (Abraham) stomper.fetch();
			if (each == null) {
				continue ;
			}
			each.newShepherd();
		}
		stomper.destroy();
	}
	if (((Boolean) InsideAgenda.fluidFetch()).booleanValue()) {
		return ;
	}
	agenda = ((Turtle) myInitialFlock).fetchAgenda();
	if (agenda != null) {
		Object insideAgendaOldValue = AboraBlockSupport.enterFluidBindDuring(InsideAgenda, true);
		try {
			while (agenda.step());
		}
		finally {
			AboraBlockSupport.exitFluidBindDuring(InsideAgenda, insideAgendaOldValue);
		}
	}
	if (myDestroyedFlocks.isEmpty()) {
		return ;
	}
	Object insideAgendaOldValue1 = AboraBlockSupport.enterFluidBindDuring(InsideAgenda, true);
	try {
		while ( ! (myDestroyedFlocks.isEmpty())) {
			Abraham flock;
			flock = (Abraham) (myDestroyedFlocks.intGet(myDestroyedFlocks.count() - 1));
			myDestroyedFlocks.intRemove(myDestroyedFlocks.count() - 1);
			if (flock.getInfo().isForgotten()) {
				flock.dismantle();
			}
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(InsideAgenda, insideAgendaOldValue1);
	}
/*
udanax-top.st:18023:TestPacker methodsFor: 'transactions'!
{void} endConsistent: dirty {IntegerVar unused} 
	| agenda {Agenda | NULL} |
	
	InsideTransactionFlag fluidFetch ifTrue: [^VOID].
	myAlmostNewFlocks isEmpty
		ifFalse: 
			[blastOnError
				ifTrue: [Heaper BLAST: #MustDoNewShepherdAfterDiskUpdate].
			cerr << 'These flocks should have done a newShepherd: ' << myAlmostNewFlocks << '
'.
			myAlmostNewFlocks stepper forEach: [:each {Abraham} | 
				each newShepherd]].
		
	InsideAgenda fluidFetch ifTrue: [^VOID].
	agenda _ (myInitialFlock cast: Turtle) fetchAgenda.
	agenda ~~ NULL ifTrue: 
		[InsideAgenda fluidBind: true during:
			[[agenda step] whileTrue]].
	myDestroyedFlocks isEmpty ifTrue: [^VOID].
	InsideAgenda fluidBind: true during:
		[[myDestroyedFlocks isEmpty]
			whileFalse:
				[| flock {Abraham} |
				flock _ (myDestroyedFlocks intGet: myDestroyedFlocks count - 1) cast: Abraham.
				myDestroyedFlocks intRemove: myDestroyedFlocks count - 1.
				flock getInfo isForgotten ifTrue: [flock dismantle]]]!
*/
}
public boolean insideCommit() {
	return amCommitting;
/*
udanax-top.st:18051:TestPacker methodsFor: 'transactions'!
{BooleanVar} insideCommit
	^ amCommitting!
*/
}
public void makePersistent() {
	try {
		amCommitting = true;
		refitFlocks();
		Stepper stomper = myNewFlocks.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FlockInfo info = (FlockInfo) stomper.fetch();
			if (info == null) {
				continue ;
			}
			Abraham shep;
			if ((shep = info.fetchShepherd()) != null) {
				assignSnarf(shep);
			}
		}
		stomper.destroy();
		flushChanges();
		myNewFlocks.destroy();
		myNewFlocks = IntegerTable.make(500);
	}
	finally {
		TestPacker.bombEndCommit(this);
	}
/*
udanax-top.st:18054:TestPacker methodsFor: 'transactions'!
{void} makePersistent
	[amCommitting := true.
	self refitFlocks.
	myNewFlocks stepper forEach: [:info {FlockInfo} | | shep {Abraham} |
		(shep _ info fetchShepherd) ~~ NULL ifTrue:
			[self assignSnarf: shep]].
	self flushChanges.
	myNewFlocks destroy.
	myNewFlocks := IntegerTable make: 500]
		valueNowOrOnUnwindDo:
			(TestPacker bomb.EndCommit: self)!
*/
}
public void purge() {
	if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue())) {
		makePersistent();
		purgeClean(true);
	}
/*
udanax-top.st:18067:TestPacker methodsFor: 'transactions'!
{void} purge
	InsideTransactionFlag fluidFetch ifFalse:
		[self makePersistent.
		self purgeClean: true]!
*/
}
public void purgeClean(boolean noneLocked) {
	PrimPtrTable stackPtrs;
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
		if (shep != null && ( ! shep.isStub() && ((stackPtrs.fetch(shep.asOop())) == null && (shep.isPurgeable() && ( ! info.isDirty()))))) {
			shep.becomeStub();
		}
		/* Removed translateOnly */
	}
	stomper.destroy();
	AboraSupport.logger.print("done.");
	AboraSupport.logger.println();
/*
udanax-top.st:18073:TestPacker methodsFor: 'transactions'!
{void} purgeClean: noneLocked {BooleanVar default: false}
	
	 | stackPtrs {PrimPtrTable} |
	[Transcript show: 'Starting purge...'] smalltalkOnly.
	noneLocked
		ifTrue: [stackPtrs _ PrimPtrTable make: 1]
		ifFalse: [stackPtrs _ StackExaminer pointersOnStack].
	myFlocks stepper forEach:
		[ :info {FlockInfo} |
		| shep {Abraham} |
		shep := info fetchShepherd.
		[(shep ~~ NULL
				and: [shep isStub not
				and: [(stackPtrs fetch: shep asOop) == NULL
				and: [shep isPurgeable
				and: [info isDirty not]]]])
			ifTrue: [shep becomeStub]] smalltalkOnly.
		'if (shep && shep->fetchInfo() == info && !!shep->isStub() && (stackPtrs->fetch((Int32)(void*)shep) == NULL) && shep->isPurgeable() && !!info->isDirty()) {
			shep->becomeStub();
			}' translateOnly.].
	[Transcript show: 'done.'; cr] smalltalkOnly!
*/
}
/**
 * @deprecated
 */
public void makeConsistent() {
	throw new PasseException();
/*
udanax-top.st:18097:TestPacker methodsFor: 'smalltalk: passe'!
{void} makeConsistent
	self passe.
	myAlmostNewFlocks isEmpty ifFalse:
		[blastOnError ifTrue:
			[Heaper BLAST: #MustDoNewShepherdAfterDiskUpdate].
		cerr << 'These flocks should have done a newShepherd: ' << myAlmostNewFlocks << '
'.
		myAlmostNewFlocks stepper forEach: [ :each {Abraham} |
			each newShepherd]].
	self countDown < IntegerVar0 ifTrue: 
		[self makePersistent.
		self resetCountDown]!
*/
}
public boolean isFake() {
	return false;
/*
udanax-top.st:18113:TestPacker methodsFor: 'testing'!
{BooleanVar} isFake
	^ false!
*/
}
public static void bombEndCommit(TestPacker CHARGE) {
	CHARGE.committing(false);
/*
udanax-top.st:18127:TestPacker class methodsFor: 'exceptions: private:'!
bomb.EndCommit: CHARGE {TestPacker star}
	^[CHARGE committing: false]!
*/
}
public static DiskManager make(boolean blast, int persistInterval) {
	DiskManager result;
	result = new TestPacker(blast, persistInterval);
	CurrentPacker.fluidSet(result);
	return result;
/*
udanax-top.st:18133:TestPacker class methodsFor: 'pseudo constructors'!
{DiskManager} make: blast {BooleanVar} with: persistInterval {IntegerVar}
	| result {DiskManager} |
	result := self create: blast with: persistInterval.
	CurrentPacker fluidSet: result.
	^result!
*/
}
public TestPacker() {
/*

Generated during transformation
*/
}
public TestPacker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
