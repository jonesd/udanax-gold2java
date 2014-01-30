/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.be.locks.ChallengeLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.be.locks.MatchLock;
import info.dgjones.abora.gold.be.locks.MultiLock;
import info.dgjones.abora.gold.be.locks.WallLock;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimFloatArray;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.cross.CrossMapping;
import info.dgjones.abora.gold.cross.CrossOrderSpec;
import info.dgjones.abora.gold.detect.FeDetector;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.detect.FeRevisionDetector;
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterPosition;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.Logger;
import info.dgjones.abora.gold.java.missing.Problem;
import info.dgjones.abora.gold.java.missing.handle.BHFn;
import info.dgjones.abora.gold.java.missing.handle.BHHFn;
import info.dgjones.abora.gold.java.missing.handle.BHHHFn;
import info.dgjones.abora.gold.java.missing.handle.HFn;
import info.dgjones.abora.gold.java.missing.handle.HHBFn;
import info.dgjones.abora.gold.java.missing.handle.HHFn;
import info.dgjones.abora.gold.java.missing.handle.HHHBFn;
import info.dgjones.abora.gold.java.missing.handle.HHHFn;
import info.dgjones.abora.gold.java.missing.handle.HHHHFn;
import info.dgjones.abora.gold.java.missing.handle.HHHHHFn;
import info.dgjones.abora.gold.java.missing.handle.HHHHHHFn;
import info.dgjones.abora.gold.java.missing.handle.HHHHHHHFn;
import info.dgjones.abora.gold.java.missing.handle.VHBFn;
import info.dgjones.abora.gold.java.missing.handle.VHFn;
import info.dgjones.abora.gold.java.missing.handle.VHHFn;
import info.dgjones.abora.gold.java.missing.handle.VHHHFn;
import info.dgjones.abora.gold.java.missing.handle.VHHHHFn;
import info.dgjones.abora.gold.java.missing.handle.VHHHHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Array;
import info.dgjones.abora.gold.java.missing.smalltalk.Dictionary;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeBooLockSmith;
import info.dgjones.abora.gold.nadmin.FeChallengeLockSmith;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeMatchLockSmith;
import info.dgjones.abora.gold.nadmin.FeMultiLockSmith;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nkernel.FeAdminer;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeIDHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FePlaceHolderBundle;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nlinks.FeHyperLink;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.nlinks.FeMultiRef;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.nlinks.FeSingleRef;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.primtab.PrimIndexTableStepper;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.proman.BHHHHandler;
import info.dgjones.abora.gold.proman.BHHHandler;
import info.dgjones.abora.gold.proman.BHHandler;
import info.dgjones.abora.gold.proman.ByteShuffler;
import info.dgjones.abora.gold.proman.CommFillDetector;
import info.dgjones.abora.gold.proman.CommFillRangeDetector;
import info.dgjones.abora.gold.proman.CommRevisionDetector;
import info.dgjones.abora.gold.proman.CommStatusDetector;
import info.dgjones.abora.gold.proman.CommWaitDetector;
import info.dgjones.abora.gold.proman.DetectorEvent;
import info.dgjones.abora.gold.proman.ExceptionRecord;
import info.dgjones.abora.gold.proman.HHBHandler;
import info.dgjones.abora.gold.proman.HHHBHandler;
import info.dgjones.abora.gold.proman.HHHHHHHHandler;
import info.dgjones.abora.gold.proman.HHHHHHHandler;
import info.dgjones.abora.gold.proman.HHHHHHandler;
import info.dgjones.abora.gold.proman.HHHHHandler;
import info.dgjones.abora.gold.proman.HHHHandler;
import info.dgjones.abora.gold.proman.HHHandler;
import info.dgjones.abora.gold.proman.HHandler;
import info.dgjones.abora.gold.proman.NoShuffler;
import info.dgjones.abora.gold.proman.Portal;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.SimpleShuffler;
import info.dgjones.abora.gold.proman.SpecialHandler;
import info.dgjones.abora.gold.proman.VHBHandler;
import info.dgjones.abora.gold.proman.VHHHHHHandler;
import info.dgjones.abora.gold.proman.VHHHHHandler;
import info.dgjones.abora.gold.proman.VHHHHandler;
import info.dgjones.abora.gold.proman.VHHHandler;
import info.dgjones.abora.gold.proman.VHHandler;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.sysadm.FeArchiver;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.tumbler.RealRegion;
import info.dgjones.abora.gold.tumbler.RealSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceMapping;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeText;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class PromiseManager extends Heaper {

	protected Portal myPortal;
	protected XnReadStream myReadStream;
	protected XnWriteStream myWriteStream;
	protected PrimPtrTable myActuals;
	protected PrimIndexTable myRefCounts;
	protected DetectorEvent myDetectorEvents;
	protected int myNextClientPromise;
	protected int myNextServerPromise;
	protected PtrArray myHandlers;
	protected int myAcks;
	protected ExceptionRecord myError;
	protected boolean amInsideRequest;
	protected ByteShuffler myShuffler;
	protected static PtrArray AllRequests;
	protected static PtrArray LoginRequests;
	protected static Array OverrideArray;
	protected static Dictionary OverrideMap;
	protected static PtrArray PromiseClasses;
/*
udanax-top.st:35175:
Heaper subclass: #PromiseManager
	instanceVariableNames: '
		myPortal {Portal}
		myReadStream {XnReadStream}
		myWriteStream {XnWriteStream}
		myActuals {PrimPtrTable}
		myRefCounts {PrimIndexTable}
		myDetectorEvents {DetectorEvent}
		myNextClientPromise {IntegerVar}
		myNextServerPromise {IntegerVar}
		myHandlers {PtrArray}
		myAcks {IntegerVar}
		myError {ExceptionRecord}
		amInsideRequest {BooleanVar}
		myShuffler {ByteShuffler}'
	classVariableNames: '
		AllRequests {PtrArray of: RequestHandler} 
		LoginRequests {PtrArray of: RequestHandler} 
		OverrideArray {Array smalltalk} 
		OverrideMap {Dictionary smalltalk} 
		PromiseClasses {PtrArray of: Category} '
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:35197:
(PromiseManager getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:35694:
PromiseManager class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:35697:
(PromiseManager getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PromiseManager.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void force() {
	flushAcks();
	if (myDetectorEvents != null) {
		while (myDetectorEvents != null) {
			myDetectorEvents.trigger(this);
			myDetectorEvents = myDetectorEvents.next();
		}
	}
	myWriteStream.flush();
/*
udanax-top.st:35202:PromiseManager methodsFor: 'operations'!
{void} force
	self flushAcks.
	myDetectorEvents ~~ NULL ifTrue:
		[[myDetectorEvents ~~ NULL] whileTrue:
			[myDetectorEvents trigger: self.
			myDetectorEvents _ myDetectorEvents next]].
	myWriteStream flush!
*/
}
public void handleRequest() {
	amInsideRequest = true;
	Someone.thingToDo();
	/* This should not forward all errors. */
	try {
		int reqnum;
		reqnum = receiveRequestNumber();
		/* [cerr cr << myNextClientPromise << (myHandlers get: reqnum).  cerr endEntry] smalltalkOnly. */
		((RequestHandler) (myHandlers.get(reqnum))).handleRequest(this);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.ALLUBUT.equals(ex.getMessage()) || AboraRuntimeException.MEMUALLOCUERROR.equals(ex.getMessage()) || AboraRuntimeException.NULLUCHKPTR.equals(ex.getMessage()) || AboraRuntimeException.NULL_RESPONSE_RESULT.equals(ex.getMessage()) || AboraRuntimeException.PUREUVIRTUAL.equals(ex.getMessage()) || AboraRuntimeException.SANITY_VIOLATION.equals(ex.getMessage()) || AboraRuntimeException.SOCKETURECVUERROR.equals(ex.getMessage()) || AboraRuntimeException.SOCKETUSENDUERROR.equals(ex.getMessage()) || AboraRuntimeException.SUBCLASSURESPONSIBILITY.equals(ex.getMessage()) || AboraRuntimeException.URDIUJACKPOT.equals(ex.getMessage())) {
			Problem prob;
			AboraSupport.translateOnly();
			{
				/* prob = &PROBLEM(ex); */
			}
			AboraSupport.smalltalkOnly();
			{
				prob = new Problem(ex.PROBLEM(), ex.parameter(), ex.initialContext().sender().printString(), 0);
			}
			respondProblem(prob);
		}
		else {
			throw ex;
		}
	}
	if (myError != null) {
		respondError();
	}
	amInsideRequest = false;
	if (myDetectorEvents != null
	/* Forcing flushes detector events too. */
	) {
		force();
	}
	else {
		myWriteStream.flush();
	}
/*
udanax-top.st:35210:PromiseManager methodsFor: 'operations'!
{void} handleRequest
	amInsideRequest _ true.
	self thingToDo.  "This should not forward all errors."
	PromiseManager problems.EVERY.U.COMM
		handle: 
			[:ex | 
			| prob {Problem} |
			'prob = &PROBLEM(ex);' translateOnly.
			[prob _ Problem create: ex PROBLEM with: ex parameter with: ex initialContext sender printString with: 0] smalltalkOnly.
			self respondProblem: prob.
			ex return]
		do: [| reqnum {Int32} |
			reqnum _ self receiveRequestNumber.
			"[cerr cr << myNextClientPromise << (myHandlers get: reqnum).  cerr endEntry] smalltalkOnly."
			((myHandlers get: reqnum) cast: RequestHandler) handleRequest: self].
	myError ~~ NULL ifTrue: [self respondError].
	amInsideRequest _ false.
	myDetectorEvents ~~ NULL   "Forcing flushes detector events too."
		ifTrue: [self force]
		ifFalse: [myWriteStream flush]!
*/
}
/**
 * Return true if no errors have occurred in the current transaction.
 */
public boolean noErrors() {
	return myError == null;
/*
udanax-top.st:35231:PromiseManager methodsFor: 'operations'!
{BooleanVar} noErrors
	"Return true if no errors have occurred in the current transaction."
	^myError == NULL!
*/
}
/**
 * Queue up the detector event.  It will be executed after the next transaction.
 */
public void queueDetectorEvent(DetectorEvent event) {
	event.setNext(myDetectorEvents);
	myDetectorEvents = event;
	if ( ! (amInsideRequest)) {
		force();
	}
/*
udanax-top.st:35236:PromiseManager methodsFor: 'operations'!
{void} queueDetectorEvent: event {DetectorEvent}
	"Queue up the detector event.  It will be executed after the next transaction."
	
	event setNext: myDetectorEvents.
	myDetectorEvents _ event.
	amInsideRequest ifFalse: [self force]!
*/
}
/**
 * Release the promise argument.  This could return a value because the PromiseManager
 * doesn't keep any state for void promises.
 */
public void waive() {
	actualWaive(receiveIntegerVar());
/*
udanax-top.st:35243:PromiseManager methodsFor: 'operations'!
{void} waive
	"Release the promise argument.  This could return a value because the PromiseManager doesn't keep any state for void promises."
	self actualWaive: self receiveIntegerVar!
*/
}
/**
 * Release a range of promise argument, given a start and a count.  This could return a value
 * because the PromiseManager doesn't keep any state for void promises.
 */
public void waiveMany() {
	int prnum;
	int prcount;
	prnum = receiveIntegerVar();
	prcount = receiveIntegerVar();
	for (int i = prnum; i < prnum + prcount; i ++ ) {
		actualWaive(i);
	}
/*
udanax-top.st:35248:PromiseManager methodsFor: 'operations'!
{void} waiveMany
	"Release a range of promise argument, given a start and a count.  This could return a value because the PromiseManager doesn't keep any state for void promises."
	| prnum {IntegerVar} prcount {IntegerVar} |
	prnum _ self receiveIntegerVar.
	prcount _ self receiveIntegerVar.
	prnum almostTo: prnum + prcount do: [ :i {IntegerVar} |
		self actualWaive: i]!
*/
}
/**
 * Optimize promise arguments that are expected to point at IntValues.
 */
public boolean fetchBooleanVar() {
	int num;
	Heaper result;
	num = receiveIntegerVar();
	result = myActuals.fetch(num);
	if (result == null) {
		myError = (ExceptionRecord.excuse(num)).best(myError);
		return false;
	}
	if (result instanceof PrimIntValue) {
		PrimIntValue number = (PrimIntValue) result;
		return number.asIntegerVar() != 0;
	}
	else {
		myError = (ExceptionRecord.mismatch(num)).best(myError);
	}
	return false;
/*
udanax-top.st:35259:PromiseManager methodsFor: 'comm'!
{BooleanVar} fetchBooleanVar
	"Optimize promise arguments that are expected to point at IntValues."
	| num {IntegerVar} result {Heaper | NULL} |
	num _ self receiveIntegerVar.
	result _ myActuals fetch: num.
	result == NULL ifTrue: 
		[myError _ (ExceptionRecord excuse: num) best: myError.
		^false].
	result cast: PrimIntValue into: [:number | ^number asIntegerVar ~~ IntegerVarZero]
		others: [myError _ (ExceptionRecord mismatch: num) best: myError].
	^false!
*/
}
public Category fetchCategory() {
	int num;
	Category result;
	Someone.thingToDo();
	/* Renumber the categories from 0. */
	num = receiveRequestNumber();
	result = (Category) (PromiseClasses.fetch(num));
	if (result == null) {
		myError = (ExceptionRecord.badCategory(num)).best(myError);
		return null;
	}
	return result;
/*
udanax-top.st:35271:PromiseManager methodsFor: 'comm'!
{Category | NULL} fetchCategory
	| num {Int32} result {Category | NULL} |
	
	self thingToDo.  "Renumber the categories from 0." 
	num _ self receiveRequestNumber.
	result _ (PromiseClasses fetch: num) cast: Category.
	result == NULL ifTrue: 
		[myError _ (ExceptionRecord badCategory: num) best: myError.
		^NULL].
	^result!
*/
}
public Heaper fetchHeaper(Category cat) {
	int num;
	Heaper result;
	num = receiveIntegerVar();
	if (num == 0) {
		return null;
	}
	result = myActuals.fetch(num);
	if (result == null) {
		myError = (ExceptionRecord.excuse(num)).best(myError);
		return null;
	}
	if ( ! (result.isKindOf(cat))) {
		myError = (ExceptionRecord.mismatch(num)).best(myError);
		return null;
	}
	return result;
/*
udanax-top.st:35282:PromiseManager methodsFor: 'comm'!
{Heaper} fetchHeaper: cat {Category}
	| num {IntegerVar} result {Heaper | NULL} |
	num _ self receiveIntegerVar.
	num == IntegerVarZero ifTrue: [^NULL].
	result _ myActuals fetch: num.
	result == NULL ifTrue: 
		[myError _ (ExceptionRecord excuse: num) best: myError.
		^NULL].
	(result isKindOf: cat) ifFalse: 
		[myError _ (ExceptionRecord mismatch: num) best: myError.
		^NULL].
	^result!
*/
}
public int fetchInt32() {
	return fetchIntegerVar();
/*
udanax-top.st:35295:PromiseManager methodsFor: 'comm'!
{Int32} fetchInt32
	^self fetchIntegerVar DOTasLong!
*/
}
/**
 * Optimize promise arguments that are expected to point at IntValues.
 */
public int fetchIntegerVar() {
	int num;
	Heaper result;
	num = receiveIntegerVar();
	result = myActuals.fetch(num);
	if (result == null) {
		myError = (ExceptionRecord.excuse(num)).best(myError);
		return 0;
	}
	if (result instanceof PrimIntValue) {
		PrimIntValue number = (PrimIntValue) result;
		return number.asIntegerVar();
	}
	else {
		myError = (ExceptionRecord.mismatch(num)).best(myError);
	}
	return 0;
/*
udanax-top.st:35299:PromiseManager methodsFor: 'comm'!
{IntegerVar} fetchIntegerVar
	"Optimize promise arguments that are expected to point at IntValues."
	| num {IntegerVar} result {Heaper | NULL} |
	num _ self receiveIntegerVar.
	result _ myActuals fetch: num.
	result == NULL ifTrue: 
		[myError _ (ExceptionRecord excuse: num) best: myError.
		^IntegerVarZero].
	result cast: PrimIntValue into: [:number | ^number asIntegerVar]
		others: [myError _ (ExceptionRecord mismatch: num) best: myError].
	^IntegerVarZero!
*/
}
public Heaper fetchNonNullHeaper(Category cat) {
	int num;
	Heaper result;
	num = receiveIntegerVar();
	if (num == 0) {
		myError = (ExceptionRecord.wasNull(num)).best(myError);
		return null;
	}
	result = myActuals.fetch(num);
	if (result == null) {
		myError = (ExceptionRecord.excuse(num)).best(myError);
		return null;
	}
	if ( ! (result.isKindOf(cat))) {
		myError = (ExceptionRecord.mismatch(num)).best(myError);
		return null;
	}
	return result;
/*
udanax-top.st:35311:PromiseManager methodsFor: 'comm'!
{Heaper} fetchNonNullHeaper: cat {Category}
	| num {IntegerVar} result {Heaper | NULL} |
	num _ self receiveIntegerVar.
	num == IntegerVarZero ifTrue:
		[myError _ (ExceptionRecord wasNull: num) best: myError.
		^NULL].
	result _ myActuals fetch: num.
	result == NULL ifTrue: 
		[myError _ (ExceptionRecord excuse: num) best: myError.
		^NULL].
	(result isKindOf: cat) ifFalse: 
		[myError _ (ExceptionRecord mismatch: num) best: myError.
		^NULL].
	^result!
*/
}
/**
 * A new representation that requires less shifting (eventually).
 */
public int receiveIntegerVar() {
	/* 
7/1 		0<7>
14/2	10<6>		<8>
21/3	110<5>		<16>
28/4	1110<4>		<24>
35/5	11110<3>	<32>
42/6	111110<2>	<40>
49/7	1111110<1>	<48>
56/8	11111110 	<56>
+/+	11111111  <humber count>
 */
	/* This is smalltalk only because smalltalk doesn't do sign-extend. */
	int bytex;
	int mask;
	int count;
	int num;
	/* count is bytes following first word or -1 if bignum meaning next byte is humber for actual count */
	bytex = myReadStream.getByte();
	if (bytex <= 63) {
		return bytex;
	}
	if (bytex <= 127) {
		return bytex-128;
	}
	if (bytex <= 191) {
		mask = 63;
		count = 1;
	}
	else {
		if (bytex <= 223) {
			mask = 31;
			count = 2;
		}
		else {
			if (bytex <= 239) {
				mask = 15;
				count = 3;
			}
			else {
				if (bytex <= 247) {
					mask = 7;
					count = 4;
				}
				else {
					throw new UnimplementedException();
				}
			}
		}
	}
	bytex = bytex & mask;
	if ((bytex & (( ~ mask >> 1) & mask)) != 0) {
		bytex = bytex | ~ mask;
		num = -1;
	}
	else {
		num = 0;
		if ((count > 3) && ((bytex != (bytex & mask)))) {
			throw new UnimplementedException();
		}
	}
	num = (num << 8) + bytex;
	for (int i = 1; i <= count; i ++ ) {
		num = (num << 8) + myReadStream.getByte();
	}
	return num;
/*
udanax-top.st:35326:PromiseManager methodsFor: 'comm'!
{IntegerVar} receiveIntegerVar
	"A new representation that requires less shifting (eventually)."
"
7/1 		0<7>
14/2	10<6>		<8>
21/3	110<5>		<16>
28/4	1110<4>		<24>
35/5	11110<3>	<32>
42/6	111110<2>	<40>
49/7	1111110<1>	<48>
56/8	11111110 	<56>
+/+	11111111  <humber count>
"
"This is smalltalk only because smalltalk doesn't do sign-extend."
	| byte {UInt8} mask {UInt8} count {Int32} num {Int32} |
	"count is bytes following first word or -1 if bignum meaning next byte is humber for actual count"
	byte _  myReadStream getByte.
	byte <= 2r00111111 ifTrue: [^byte]. 
	byte <= 2r01111111 ifTrue: [^byte-128].
	byte <= 2r10111111
		ifTrue: [mask _ 2r00111111.  count _ 1]
		ifFalse: [byte <= 2r11011111
					ifTrue: [mask _ 2r00011111.  count _ 2]
					ifFalse: [byte <= 2r11101111
								ifTrue: [mask _ 2r00001111.  count _ 3]
								ifFalse: [byte <= 2r11110111
											ifTrue: [mask _ 2r00000111.  count _ 4]
											ifFalse: [self unimplemented]]]].
	byte _ byte bitAnd: mask.
	(byte bitAnd: ((mask bitInvert bitShiftRight: 1) bitAnd: mask)) ~= Int32Zero
		ifTrue:
			[byte _ byte bitOr: mask bitInvert.
			num _ -1]
		ifFalse: [num _ Int32Zero.
				((count > 3) and: [(byte ~= (byte bitAnd: mask))]) ifTrue: [self unimplemented]].
	num _ (num bitShift: 8) + byte.
	1 to: count do: [:i {Int32} | num _ (num bitShift: 8) + myReadStream getByte].
	^ num!
*/
}
public void respondBooleanVar(boolean val) {
	if (myError == null) {
		flushAcks();
		sendResponse(PromiseManager.humberResponse());
		/* Removed translateOnly */
		sendIntegerVar(((val) ? 1 : 0));
		myActuals.introduce(myNextClientPromise, (PrimIntValue.make(((val) ? 1 : 0))));
		myNextClientPromise = myNextClientPromise + 1;
	}
/*
udanax-top.st:35365:PromiseManager methodsFor: 'comm'!
{void} respondBooleanVar: val {BooleanVar}
	myError == NULL ifTrue:
		[self flushAcks.
		self sendResponse: PromiseManager humberResponse.
		[self sendIntegerVar: val.
		myActuals at: myNextClientPromise introduce: (PrimIntValue make: val)] translateOnly.
		[self sendIntegerVar: (val ifTrue: [1] ifFalse: [0]).
		myActuals at: myNextClientPromise introduce: (PrimIntValue make: (val ifTrue: [1] ifFalse: [0]))] smalltalkOnly.
		myNextClientPromise _ myNextClientPromise + 1]!
*/
}
public void respondHeaper(Heaper result) {
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_RESPONSE_RESULT);
	}
	if (myError == null) {
		if (result instanceof PrimIntValue) {
			PrimIntValue i = (PrimIntValue) result;
			flushAcks();
			sendResponse(PromiseManager.humberResponse());
			sendIntegerVar(i.asIntegerVar());
		}
		else if (result instanceof PrimFloatValue) {
			PrimFloatValue f = (PrimFloatValue) result;
			flushAcks();
			sendResponse(PromiseManager.IEEEResponse());
			if (f.bitCount() == 64) {
				sendIEEE64(f.asIEEE64());
			}
			else {
				if (f.bitCount() == 32) {
					sendIEEE32(f.asIEEE32());
				}
				else {
					throw new UnimplementedException();
				}
			}
		}
		else {
			myAcks = myAcks + 1;
		}
		myActuals.introduce(myNextClientPromise, result);
		if (result instanceof FeDetector) {
			int refCt;
			refCt = myRefCounts.fetch(result);
			if (refCt == -1) {
				myRefCounts.introduce(result, 1);
			}
			else {
				myRefCounts.remove(result);
				myRefCounts.introduce(result, refCt + 1);
			}
		}
		myNextClientPromise = myNextClientPromise + 1;
	}
/*
udanax-top.st:35375:PromiseManager methodsFor: 'comm'!
{void} respondHeaper: result {Heaper}
	result == NULL ifTrue:
		[Heaper BLAST: #NullResponseResult].
	myError == NULL ifTrue:
		[result cast: PrimIntValue into: [ :i |
			self flushAcks.
			self sendResponse: PromiseManager humberResponse.
			self sendIntegerVar: i asIntegerVar]
		cast: PrimFloatValue into: [ :f |
			self flushAcks.
			self sendResponse: PromiseManager IEEEResponse.
			f bitCount = 64 ifTrue:
				[self sendIEEE64: f asIEEE64]
			ifFalse: [f bitCount = 32 ifTrue:
				[self sendIEEE32: f asIEEE32]
			ifFalse:
				[self unimplemented]]]
		others: [myAcks _ myAcks + 1].
		myActuals at: myNextClientPromise introduce: result.
		(result isKindOf: FeDetector) ifTrue: 
			[| refCt {IntegerVar} |
			refCt _ myRefCounts fetch: result.
			refCt == -1
				ifTrue: [myRefCounts at: result introduce: 1]
				ifFalse: 
					[myRefCounts remove: result.
					myRefCounts at: result introduce: refCt + 1]].
		myNextClientPromise _ myNextClientPromise + 1]!
*/
}
public void respondIntegerVar(int val) {
	if (myError == null) {
		flushAcks();
		sendResponse(PromiseManager.humberResponse());
		sendIntegerVar(val);
		myActuals.introduce(myNextClientPromise, (PrimIntValue.make(val)));
		myNextClientPromise = myNextClientPromise + 1;
	}
/*
udanax-top.st:35404:PromiseManager methodsFor: 'comm'!
{void} respondIntegerVar: val {IntegerVar}
	myError == NULL ifTrue:
		[self flushAcks.
		self sendResponse: PromiseManager humberResponse.
		self sendIntegerVar: val.
		myActuals at: myNextClientPromise introduce: (PrimIntValue make: val).
		myNextClientPromise _ myNextClientPromise + 1]!
*/
}
public void respondVoid() {
	if (myError == null) {
		myAcks = myAcks + 1;
		myNextClientPromise = myNextClientPromise + 1;
	}
/*
udanax-top.st:35412:PromiseManager methodsFor: 'comm'!
{void} respondVoid
	myError == NULL ifTrue:
		[myAcks _ myAcks + 1.
		myNextClientPromise _ myNextClientPromise + 1]!
*/
}
public void sendIEEE32(float f) {
	AboraSupport.translateOnly();
	{
		/* this->sendIntegerVar (4);
	for (UInt32 i = 0; i < 4; i++) {
		myWriteStream->putByte (((UInt8 *) &f) [i]);
	} */
	}
	AboraSupport.smalltalkOnly();
	{
		throw new UnimplementedException();
	}
/*
udanax-top.st:35417:PromiseManager methodsFor: 'comm'!
{void} sendIEEE32: f {IEEE32}
	'this->sendIntegerVar (4);
	for (UInt32 i = 0; i < 4; i++) {
		myWriteStream->putByte (((UInt8 *) &f) [i]);
	}' translateOnly.
	[self unimplemented] smalltalkOnly.!
*/
}
public void sendIEEE64(double f) {
	AboraSupport.translateOnly();
	{
		/* this->sendIntegerVar (8);
	for (UInt32 i = 0; i < 8; i++) {
		myWriteStream->putByte (((UInt8 *) &f) [i]);
	} */
	}
	AboraSupport.smalltalkOnly();
	{
		throw new UnimplementedException();
	}
/*
udanax-top.st:35425:PromiseManager methodsFor: 'comm'!
{void} sendIEEE64: f {IEEE64}
	'this->sendIntegerVar (8);
	for (UInt32 i = 0; i < 8; i++) {
		myWriteStream->putByte (((UInt8 *) &f) [i]);
	}' translateOnly.
	[self unimplemented] smalltalkOnly.!
*/
}
/**
 * Send a Dean style humber.  Like Drexler style, except all the tag bits go into the first
 * byte.
 */
public void sendIntegerVar(int num) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:35433:PromiseManager methodsFor: 'comm'!
{void} sendIntegerVar: num {IntegerVar}
	"Send a Dean style humber.  Like Drexler style, except all the tag bits go into the first byte."
	 
"
7/1 		0<7>
14/2	10<6>		<8>
21/3	110<5>		<16>
28/4	1110<4>		<24>
35/5	11110<3>	<32>
42/6	111110<2>	<40>
49/7	1111110<1>	<48>
56/8	11111110 	<56>
+/+	11111111  <humber count>
"
	| abs {IntegerVar} low32 {Int32} |
	num < IntegerVarZero ifTrue: [abs _ num negated] ifFalse: [abs _ num].
	low32 _ (num bitAnd: ("(1 bitShift: 32) -1"  4294967295)) DOTasLong.
	num < "1 bitShift: 6" 64 ifTrue: [myWriteStream putByte: (low32 bitAnd: 127).  ^VOID].
	abs < "1 bitShift: 13"  8192 ifTrue: 
		[myWriteStream putByte: (((low32 bitShiftRight: 8) bitAnd: 2r0111111) bitOr: 2r10000000).
		myWriteStream putByte: (low32 bitAnd: 255).
		^VOID].
	abs < "1 bitShift: 20"  1048576 ifTrue: 
		[myWriteStream putByte: (((low32 bitShiftRight: 16) bitAnd: 2r011111) bitOr: 2r11000000).
		myWriteStream putByte: ((low32 bitShiftRight: 8) bitAnd: 255).
		myWriteStream putByte: (low32 bitAnd: 255).
		^VOID].
	abs < "1 bitShift: 27"  134217728 ifTrue: 
		[myWriteStream putByte: (((low32 bitShiftRight: 24) bitAnd: 2r00001111) bitOr: 2r11100000).
		myWriteStream putByte: ((low32 bitShiftRight: 16) bitAnd: 255).
		myWriteStream putByte: ((low32 bitShiftRight: 8) bitAnd: 255).
		myWriteStream putByte: (low32 bitAnd: 255).
		^VOID].
	"abs < (1 bitShift: 34)" true ifTrue: 
		["do shift in two steps to get around Sparc shift bug /ravi/7/23/92/"
		myWriteStream putByte: ((((num bitShiftRight: 16) bitShiftRight: 16) bitAnd: 2r0111) bitOr: 2r11110000) DOTasLong.
		myWriteStream putByte: ((num bitShiftRight: 24) bitAnd: 255) DOTasLong.
		myWriteStream putByte: ((num bitShiftRight: 16) bitAnd: 255) DOTasLong.
		myWriteStream putByte: ((num bitShiftRight: 8) bitAnd: 255) DOTasLong.
		myWriteStream putByte: (num bitAnd: 255) DOTasLong.
		^VOID].
	"self sendIntegerVar: (abs log: 256) truncated + 1."  "The humber count."
	Eric shouldImplement  "Write out each of the bytes."!
*/
}
/**
 * Register heaper with the next Server promise number and increment it.  The client must
 * stay in sync.
 */
public void sendPromise(Heaper heaper) {
	myActuals.introduce(myNextServerPromise, heaper);
	myNextServerPromise = myNextServerPromise - 1;
/*
udanax-top.st:35477:PromiseManager methodsFor: 'comm'!
{void} sendPromise: heaper {Heaper}
	"Register heaper with the next Server promise number and increment it.  The client must stay in sync."
	
	myActuals at: myNextServerPromise introduce: heaper.
	myNextServerPromise _ myNextServerPromise - 1!
*/
}
/**
 * Use a representation optimized for small positive numbers.
 */
public void sendResponse(int num) {
	/* If the number is less than 255 then just send it.  Otherwise send 255, subtract 255 and recur. */
	int val;
	val = num;
	while ( ! (num < 255)) {
		myWriteStream.putByte(255);
		val = val - 255;
	}
	myWriteStream.putByte((val));
/*
udanax-top.st:35483:PromiseManager methodsFor: 'comm'!
{void} sendResponse: num {Int32}
	"Use a representation optimized for small positive numbers."
	"If the number is less than 255 then just send it.  Otherwise send 255, subtract 255 and recur."
	| val {Int32} |
	val _ num.
	[num < 255] whileFalse:
		[myWriteStream putByte: 255.
		val _ val - 255].
	myWriteStream putByte: (val basicCast: UInt8)!
*/
}
/**
 * Send a bunch of IntegerVars to the client.
 */
public void sendHumbers(IntegerVarArray array, int count, int start) {
	int maxx;
	maxx = start + count;
	if (maxx > array.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.OUT_OF_BOUNDS);
	}
	flushAcks();
	sendResponse(PromiseManager.humbersResponse());
	sendIntegerVar(count);
	myActuals.introduce(myNextClientPromise, (PrimIntValue.make(count)));
	for (int i = start; i < maxx; i ++ ) {
		sendIntegerVar((array.integerVarAt(i)));
	}
	myNextClientPromise = myNextClientPromise + 1;
/*
udanax-top.st:35496:PromiseManager methodsFor: 'arrays'!
{void} sendHumbers: array {IntegerVarArray} with: count {Int32} with: start {Int32}
	"Send a bunch of IntegerVars to the client."
	
	| maxx {Int32} |
	maxx _ start + count.
	maxx > array count ifTrue: [Heaper BLAST: #OutOfBounds].
	self flushAcks.
	self sendResponse: PromiseManager humbersResponse.
	self sendIntegerVar: count.
	myActuals at: myNextClientPromise introduce: (PrimIntValue make: count).
	start almostTo: maxx do: 
		[:i {Int32} | self sendIntegerVar: (array integerVarAt: i)].
	myNextClientPromise _ myNextClientPromise + 1!
*/
}
/**
 * Send a bunch of fixed precision integers to the client.
 */
public void sendIEEEs(PrimFloatArray array, int count, int start) {
	int size;
	UInt8Array buffer;
	if (start + count > array.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.OUT_OF_BOUNDS);
	}
	flushAcks();
	sendResponse(PromiseManager.IEEEsResponse());
	sendIntegerVar(array.bitCount() / 8);
	sendIntegerVar(count);
	myActuals.introduce(myNextClientPromise, (PrimIntValue.make(count)));
	size = array.bitCount() / 8 * count;
	buffer = (UInt8Array) (PrimIntArray.zeros(8, size));
	try {
		array.copyToBuffer(buffer.gutsOf(), size, count, start);
	}
	finally {
		UInt8Array.bombReleaseGuts(buffer);
	}
	try {
		myShuffler.shuffle(array.bitCount(), buffer.gutsOf(), count);
	}
	finally {
		UInt8Array.bombReleaseGuts(buffer);
	}
	myWriteStream.putData(buffer);
	myNextClientPromise = myNextClientPromise + 1;
/*
udanax-top.st:35510:PromiseManager methodsFor: 'arrays'!
{void} sendIEEEs: array {PrimFloatArray} with: count {Int32} with: start {Int32}
	"Send a bunch of fixed precision integers to the client."
	
	| size {Int32} buffer {UInt8Array} |
	start + count > array count ifTrue: [Heaper BLAST: #OutOfBounds].
	self flushAcks.
	self sendResponse: PromiseManager IEEEsResponse.
	self sendIntegerVar: array bitCount // 8.
	self sendIntegerVar: count.
	myActuals at: myNextClientPromise introduce: (PrimIntValue make: count).
	size _ array bitCount // 8 * count.
	buffer _ (PrimIntArray zeros: 8 with: size) cast: UInt8Array.
	[array copyToBuffer: buffer gutsOf with: size with: count with: start]
		valueNowOrOnUnwindDo:
			(UInt8Array bomb.ReleaseGuts: buffer).
	[myShuffler shuffle: array bitCount with: buffer gutsOf with: count]
		valueNowOrOnUnwindDo:
			(UInt8Array bomb.ReleaseGuts: buffer).
	myWriteStream putData: buffer.
	myNextClientPromise _ myNextClientPromise + 1!
*/
}
/**
 * Send a bunch of fixed precision integers to the client.
 */
public void sendInts(PrimIntArray array, int count, int start) {
	int size;
	UInt8Array buffer;
	if (start + count > array.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.OUT_OF_BOUNDS);
	}
	flushAcks();
	sendResponse(PromiseManager.intsResponse());
	sendIntegerVar(array.bitCount());
	sendIntegerVar(count);
	myActuals.introduce(myNextClientPromise, (PrimIntValue.make(count)));
	size = Math.abs(array.bitCount()) / 8 * count;
	buffer = (UInt8Array) (PrimIntArray.zeros(8, size));
	try {
		array.copyToBuffer(buffer.gutsOf(), size, count, start);
	}
	finally {
		UInt8Array.bombReleaseGuts(buffer);
	}
	try {
		myShuffler.shuffle(Math.abs(array.bitCount()), buffer.gutsOf(), count);
	}
	finally {
		UInt8Array.bombReleaseGuts(buffer);
	}
	myWriteStream.putData(buffer);
	myNextClientPromise = myNextClientPromise + 1;
/*
udanax-top.st:35531:PromiseManager methodsFor: 'arrays'!
{void} sendInts: array {PrimIntArray} with: count {Int32} with: start {Int32}
	"Send a bunch of fixed precision integers to the client."
	
	| size {Int32} buffer {UInt8Array} |
	start + count > array count ifTrue: [Heaper BLAST: #OutOfBounds].
	self flushAcks.
	self sendResponse: PromiseManager intsResponse.
	self sendIntegerVar: array bitCount.
	self sendIntegerVar: count.
	myActuals at: myNextClientPromise introduce: (PrimIntValue make: count).
	size _ array bitCount abs // 8 * count.
	buffer _ (PrimIntArray zeros: 8 with: size) cast: UInt8Array.
	[array copyToBuffer: buffer gutsOf with: size with: count with: start]
		valueNowOrOnUnwindDo:
			(UInt8Array bomb.ReleaseGuts: buffer).
	[myShuffler shuffle: array bitCount abs with: buffer gutsOf with: count]
		valueNowOrOnUnwindDo:
			(UInt8Array bomb.ReleaseGuts: buffer).
	myWriteStream putData: buffer.
	myNextClientPromise _ myNextClientPromise + 1!
*/
}
/**
 * Register heaper with the next Server promise number and increment it.  The client must
 * stay in sync.
 */
public void sendPromises(PtrArray array, int count, int start) {
	int maxx;
	int nulls;
	int ptrs;
	maxx = start + count;
	if (maxx > array.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.OUT_OF_BOUNDS);
	}
	nulls = 0;
	ptrs = 0;
	flushAcks();
	sendResponse(PromiseManager.promisesResponse());
	sendIntegerVar(count);
	myActuals.introduce(myNextClientPromise, (PrimIntValue.make(count)));
	for (int i = start; i < maxx; i ++ ) {
		Heaper elem;
		elem = array.fetch(i);
		if (elem == null) {
			if (nulls <= 0) {
				sendIntegerVar(ptrs);
				ptrs = 0;
			}
			nulls = nulls + 1;
		}
		else {
			if (nulls > 0) {
				sendIntegerVar(nulls);
				nulls = 0;
			}
			sendPromise(elem);
			ptrs = ptrs + 1;
		}
	}
	if (ptrs >= 1) {
		sendIntegerVar(ptrs);
	}
	if (nulls >= 1) {
		sendIntegerVar(nulls);
	}
	myNextClientPromise = myNextClientPromise + 1;
/*
udanax-top.st:35552:PromiseManager methodsFor: 'arrays'!
{void} sendPromises: array {PtrArray} with: count {Int32} with: start {Int32}
	"Register heaper with the next Server promise number and increment it.  The client must stay in sync."
	
	| maxx {Int32} nulls {Int32} ptrs {Int32} |
	maxx _ start + count.
	maxx > array count ifTrue: [Heaper BLAST: #OutOfBounds].
	nulls _ Int32Zero.
	ptrs _ Int32Zero.
	self flushAcks.
	self sendResponse: PromiseManager promisesResponse.
	self sendIntegerVar: count.
	myActuals at: myNextClientPromise introduce: (PrimIntValue make: count).
	start almostTo: maxx do: 
		[:i {Int32} |
		| elem {Heaper | NULL} |
		elem _ array fetch: i.
		elem == NULL
			ifTrue:
				[nulls <= Int32Zero ifTrue: 
					[self sendIntegerVar: ptrs.
					ptrs _ Int32Zero].
				nulls _ nulls + 1]
			ifFalse:
				[nulls > Int32Zero ifTrue: 
					[self sendIntegerVar: nulls.
					nulls _ Int32Zero].
				self sendPromise: elem.
				ptrs _ ptrs + 1]].
	ptrs >= 1 ifTrue: [self sendIntegerVar: ptrs].
	nulls >= 1 ifTrue: [self sendIntegerVar: nulls].
	myNextClientPromise _ myNextClientPromise + 1!
*/
}
/**
 * Release the promise argument.  This could return a value because the PromiseManager
 * doesn't keep any state for void promises.
 */
public void actualWaive(int prnum) {
	Heaper actual;
	int count;
	actual = myActuals.fetch(prnum);
	if ((myActuals.fetch(prnum)) != null) {
		myActuals.remove(prnum);
	}
	count = myRefCounts.fetch(actual);
	if (count == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.REF_COUNT_BUG);
	}
	if (count > 0) {
		if (count == 1) {
			myRefCounts.remove(actual);
			actual.destroy();
		}
		else {
			myRefCounts.introduce(actual, count - 1);
		}
	}
/*
udanax-top.st:35586:PromiseManager methodsFor: 'private: comm'!
{void} actualWaive: prnum {IntegerVar}
	"Release the promise argument.  This could return a value because the PromiseManager doesn't keep any state for void promises."
	| actual {Heaper} count {IntegerVar} |
	actual _ myActuals fetch: prnum.
	(myActuals fetch: prnum) ~~ NULL ifTrue: [myActuals remove: prnum].
	count _ myRefCounts fetch: actual.
	count == Int32Zero ifTrue: [Heaper BLAST: #RefCountBug].
	count > Int32Zero ifTrue:
		[count == 1 
			ifTrue:
				[myRefCounts remove: actual.
				actual destroy]
			ifFalse: [myRefCounts at: actual introduce: count - 1]]!
*/
}
public int clientPromiseNumber() {
	return myNextClientPromise;
/*
udanax-top.st:35601:PromiseManager methodsFor: 'private: comm'!
{IntegerVar} clientPromiseNumber
	^myNextClientPromise!
*/
}
/**
 * If any acks have accumulated, flush them.
 */
public void flushAcks() {
	if (myAcks > 0) {
		sendResponse(PromiseManager.ackResponse());
		sendIntegerVar(myAcks);
		myAcks = 0;
	}
/*
udanax-top.st:35605:PromiseManager methodsFor: 'private: comm'!
{void} flushAcks
	"If any acks have accumulated, flush them."
	
	myAcks > IntegerVarZero ifTrue:
		[self sendResponse: PromiseManager ackResponse.
		self sendIntegerVar: myAcks.
		myAcks _ IntegerVarZero]!
*/
}
public XnReadStream readStream() {
	return myReadStream;
/*
udanax-top.st:35613:PromiseManager methodsFor: 'private: comm'!
{XnReadStream} readStream
	^myReadStream!
*/
}
/**
 * Receive a request number.  The first byte is either between 0 and 254 or it
 * is 255 and the second byte + 255 is the number.
 */
public int receiveRequestNumber() {
	int bytex;
	bytex = myReadStream.getByte();
	if (bytex < 255) {
		return bytex;
	}
	return myReadStream.getByte() + 255;
/*
udanax-top.st:35616:PromiseManager methodsFor: 'private: comm'!
{Int32} receiveRequestNumber
	"Receive a request number.  The first byte is either between 0 and 254 or it
	 is 255 and the second byte + 255 is the number."
	
	| byte {Int32} |
	byte _ myReadStream getByte.
	byte < 255 ifTrue: [^byte].
	^myReadStream getByte + 255!
*/
}
public void respondError() {
	flushAcks();
	if (myError.isExcused()) {
		sendResponse(PromiseManager.excusedResponse());
		sendIntegerVar(myError.promise());
	}
	else {
		sendResponse(PromiseManager.errorResponse());
		sendIntegerVar(myError.error());
		sendIntegerVar(0);
	}
	myNextClientPromise = myNextClientPromise + 1;
	myError = null;
/*
udanax-top.st:35625:PromiseManager methodsFor: 'private: comm'!
{void} respondError
	self flushAcks.
	myError isExcused 
		ifTrue: 
			[self sendResponse: PromiseManager excusedResponse.
			self sendIntegerVar: myError promise]
		ifFalse:
			[self sendResponse: PromiseManager errorResponse.
			self sendIntegerVar: myError error.
			self sendIntegerVar: Int32Zero].
	myNextClientPromise _ myNextClientPromise + 1.
	myError _ NULL!
*/
}
public void respondProblem(Problem problem) {
	BlastLog.print("Blast sent: ");
	BlastLog.print(problem.getProblemName());
	BlastLog.print(" at: ");
	BlastLog.print(problem.getFileName());
	BlastLog.print(":");
	BlastLog.print(problem.getLineNumber());
	BlastLog.print("\n"+
"");
	flushAcks();
	sendResponse(PromiseManager.errorResponse());
	sendIntegerVar((PromiseManager.problemNumber(problem.getProblemName())));
	sendIntegerVar((PromiseManager.problemSource(problem.getFileName(), problem.getLineNumber())));
	myNextClientPromise = myNextClientPromise + 1;
	myError = null;
/*
udanax-top.st:35638:PromiseManager methodsFor: 'private: comm'!
{void} respondProblem: problem {Problem}
	BlastLog << 'Blast sent: ' << problem getProblemName << ' at: '.
	BlastLog << problem getFileName << ':' << problem getLineNumber << '
'.
	[Logger] USES.
	self flushAcks.
	self sendResponse: PromiseManager errorResponse.
	self sendIntegerVar: (PromiseManager problemNumber: problem getProblemName).
	self sendIntegerVar: (PromiseManager problemSource: problem getFileName
		with: problem getLineNumber).
	myNextClientPromise _ myNextClientPromise + 1.
	myError _ NULL!
*/
}
public int serverPromiseNumber() {
	return myNextServerPromise;
/*
udanax-top.st:35652:PromiseManager methodsFor: 'private: comm'!
{IntegerVar} serverPromiseNumber
	^myNextServerPromise!
*/
}
public PromiseManager(Portal portal, String clientID, ByteShuffler shuffler) {
	super();
	myPortal = portal;
	myReadStream = portal.readStream();
	myWriteStream = portal.writeStream();
	myActuals = PrimPtrTable.make(5000);
	myRefCounts = PrimIndexTable.make(63);
	myDetectorEvents = null;
	myNextClientPromise = 1;
	myNextServerPromise = -1;
	Someone.thingToDo();
	/* This should get a table based on the clientID. */
	myHandlers = PromiseManager.makeRequestTable();
	/* Removed clientID.delete(); */
	myAcks = 0;
	myError = null;
	amInsideRequest = false;
	myShuffler = shuffler;
/*
udanax-top.st:35658:PromiseManager methodsFor: 'protected: creation'!
create: portal {Portal} with: clientID {char star} with: shuffler {ByteShuffler}
	super create.
	myPortal _ portal.
	myReadStream _ portal readStream.
	myWriteStream _ portal writeStream.
	myActuals _ PrimPtrTable make: 5000.
	myRefCounts _ PrimIndexTable make: 63.
	myDetectorEvents _ NULL.
	myNextClientPromise _ 1.
	myNextServerPromise _ -1.
	self thingToDo.  "This should get a table based on the clientID."
	myHandlers _ PromiseManager makeRequestTable.
	clientID delete.
	myAcks _ Int32Zero.
	myError _ NULL.
	amInsideRequest _ false.
	myShuffler _ shuffler.!
*/
}
public void destruct() {
	PrimIndexTableStepper step;
	myPortal.destroy();
	/* clean up all the ref counted Detectors */
	step = myRefCounts.stepper();
	while (step.hasValue()) {
		Heaper detect;
		detect = step.key();
		detect.destroy();
		step.step();
	}
	super.destruct();
/*
udanax-top.st:35676:PromiseManager methodsFor: 'protected: creation'!
{void} destruct
	| step {PrimIndexTableStepper} |
	myPortal destroy.
	"clean up all the ref counted Detectors"
	step _ myRefCounts stepper.
	[step hasValue] whileTrue: [
		| detect {Heaper} |
		detect _ step key.
		detect destroy.
		step step].
	super destruct!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:35690:PromiseManager methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/*
udanax-top.st:35702:PromiseManager class methodsFor: 'exceptions: exceptions'!
problems.EVERY.U.COMM
	^self signals: #(ALL.U.BUT 
		SUBCLASS.U.RESPONSIBILITY 
		URDI.U.JACKPOT 
		MEM.U.ALLOC.U.ERROR
		NULL.U.CHKPTR
		PURE.U.VIRTUAL
		NullResponseResult
		SOCKET.U.RECV.U.ERROR
		SOCKET.U.SEND.U.ERROR
		SanityViolation)!
*/
public static int ackResponse() {
	return 0;
/*
udanax-top.st:35717:PromiseManager class methodsFor: 'constants'!
{Int32} ackResponse
	^Int32Zero!
*/
}
public static int doneResponse() {
	return 14;
/*
udanax-top.st:35720:PromiseManager class methodsFor: 'constants'!
{Int32} doneResponse
	^14!
*/
}
public static int errorResponse() {
	return 1;
/*
udanax-top.st:35723:PromiseManager class methodsFor: 'constants'!
{Int32} errorResponse
	^1!
*/
}
public static int excusedResponse() {
	return 2;
/*
udanax-top.st:35726:PromiseManager class methodsFor: 'constants'!
{Int32} excusedResponse
	^2!
*/
}
public static int filledResponse() {
	return 13;
/*
udanax-top.st:35729:PromiseManager class methodsFor: 'constants'!
{Int32} filledResponse
	^13!
*/
}
public static int grabbedResponse() {
	return 9;
/*
udanax-top.st:35732:PromiseManager class methodsFor: 'constants'!
{Int32} grabbedResponse
	^9!
*/
}
public static int humberResponse() {
	return 3;
/*
udanax-top.st:35735:PromiseManager class methodsFor: 'constants'!
{Int32} humberResponse
	^3!
*/
}
public static int humbersResponse() {
	return 5;
/*
udanax-top.st:35738:PromiseManager class methodsFor: 'constants'!
{Int32} humbersResponse
	^5!
*/
}
public static int IEEEResponse() {
	return 4;
/*
udanax-top.st:35741:PromiseManager class methodsFor: 'constants'!
{Int32} IEEEResponse
	^4!
*/
}
public static int IEEEsResponse() {
	return 7;
/*
udanax-top.st:35744:PromiseManager class methodsFor: 'constants'!
{Int32} IEEEsResponse
	^7!
*/
}
public static int intsResponse() {
	return 6;
/*
udanax-top.st:35747:PromiseManager class methodsFor: 'constants'!
{Int32} intsResponse
	^6!
*/
}
/**
 * The number that gets sent over the wire for the given problem name
 */
public static int problemNumber(String prob) {
	/* PromiseManager problemNumber: 'VALUE_IS_UNKIND'  */
	return (FHash.fastHashString(prob)) & 16777215;
/*
udanax-top.st:35750:PromiseManager class methodsFor: 'constants'!
{Int32} problemNumber: prob {char star}
	"The number that gets sent over the wire for the given problem name"
	"PromiseManager problemNumber: 'VALUE_IS_UNKIND' "
	
	^(FHash fastHash.String: prob) bitAnd: 16777215!
*/
}
/**
 * The number that gets sent over the wire for the given problem file/line number
 */
public static int problemSource(String file, int line) {
	return ((((FHash.fastHashString(file)) & 65535) << 15) ^ (line & 32767));
/*
udanax-top.st:35756:PromiseManager class methodsFor: 'constants'!
{Int32} problemSource: file {char star} with: line {int}
	"The number that gets sent over the wire for the given problem file/line number"
	
	^((((FHash fastHash.String: file) bitAnd: 65535) bitShift: 15) bitXor: (line bitAnd: 32767))!
*/
}
public static int promisesResponse() {
	return 8;
/*
udanax-top.st:35761:PromiseManager class methodsFor: 'constants'!
{Int32} promisesResponse
	^8!
*/
}
public static int rangeFilledResponse() {
	return 12;
/*
udanax-top.st:35764:PromiseManager class methodsFor: 'constants'!
{Int32} rangeFilledResponse
	^12!
*/
}
public static int releasedResponse() {
	return 10;
/*
udanax-top.st:35767:PromiseManager class methodsFor: 'constants'!
{Int32} releasedResponse
	^10!
*/
}
public static int revisedResponse() {
	return 11;
/*
udanax-top.st:35770:PromiseManager class methodsFor: 'constants'!
{Int32} revisedResponse
	^11!
*/
}
public static int terminatedResponse() {
	return 15;
/*
udanax-top.st:35773:PromiseManager class methodsFor: 'constants'!
{Int32} terminatedResponse
	^15!
*/
}
public static void initTimeNonInherited() {
	PromiseClasses = PtrArray.nulls(100);
	fillClassTable(PromiseClasses);
	AllRequests = PtrArray.nulls(500);
	AllRequests.storeAll((SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("NO_REQUEST_", "VHFn")))));
	fillRequestTable(AllRequests);
	/* LoginRequests _ PtrArray nulls: 500.
	LoginRequests storeAll: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #notLoginRequest: with: 'VHFn')). */
/*
udanax-top.st:35778:PromiseManager class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	PromiseClasses _ PtrArray nulls: 100.
	self fillClassTable: PromiseClasses.
	AllRequests _ PtrArray nulls: 500.
	AllRequests storeAll: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #noRequest: with: 'VHFn')).
	self fillRequestTable: AllRequests.
	"LoginRequests _ PtrArray nulls: 500.
	LoginRequests storeAll: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #notLoginRequest: with: 'VHFn'))."!
*/
}
public static void linkTimeNonInherited() {
	PromiseClasses = null;
	AllRequests = null;
	/* LoginRequests _ NULL */
	Logger.defineLogger(BLAST_LOG);
/*
udanax-top.st:35790:PromiseManager class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	PromiseClasses _ NULL.
	AllRequests _ NULL.
	"LoginRequests _ NULL"
	Logger defineLogger: #BlastLog.!
*/
}
public static PromiseManager make(Portal portal) {
	Rcvr reader;
	Xmtr writer;
	String target;
	ByteShuffler shuffler;
	reader = TextyXcvrMaker.makeReader(portal.readStream());
	writer = TextyXcvrMaker.makeWriter(portal.writeStream());
	Someone.thingToDo();
	/* Make the following loop a helper routine. */
	/* Meta-protocol */
	target = reader.receiveString();
	while ( ! ((target.compareTo("simple")) == 0)) {
		/* Removed target.delete(); */
		target = null;
		writer.sendString("no!");
		target = reader.receiveString();
	}
	writer.sendString("yes");
	/* Removed target.delete(); */
	target = null;
	/* Architecture. */
	target = reader.receiveString();
	while ( ! ((target.compareTo("sun")) == 0 || ((target.compareTo("intel")) == 0))) {
		/* Removed target.delete(); */
		target = null;
		writer.sendString("no!");
		target = reader.receiveString();
	}
	/* Removed smalltalkOnly */
	if ((target.compareTo("intel")) == 0) {
		shuffler = new SimpleShuffler();
	}
	else {
		shuffler = new NoShuffler();
	}
	/* Removed target.delete(); */
	target = null;
	writer.sendString("yes");
	/* Syntax */
	target = reader.receiveString();
	while ( ! ((target.compareTo("binary2")) == 0)) {
		/* Removed target.delete(); */
		target = null;
		writer.sendString("no!");
		target = reader.receiveString();
	}
	/* Removed target.delete(); */
	target = null;
	writer.sendString("yes");
	/* Semantics */
	target = reader.receiveString();
	while ( ! ((target.compareTo("febe92.2")) == 0)) {
		/* Removed target.delete(); */
		target = null;
		writer.sendString("no!");
		target = reader.receiveString();
	}
	writer.sendString("yes");
	writer.destroy();
	reader.destroy();
	return new PromiseManager(portal, target, shuffler);
/*
udanax-top.st:35799:PromiseManager class methodsFor: 'creation'!
make: portal {Portal} 
	| reader {Rcvr} writer {Xmtr} target {char star} shuffler {ByteShuffler} |
	reader _ TextyXcvrMaker makeReader: portal readStream.
	writer _ TextyXcvrMaker makeWriter: portal writeStream.
	self thingToDo.  "Make the following loop a helper routine."
"Meta-protocol"
	target _ reader receiveString.
	[(String strcmp: target with: 'simple') = Int32Zero] whileFalse:
		[target delete.
		target _ NULL.
		writer sendString: 'no!!'.
		target _ reader receiveString].
	writer sendString: 'yes'.
	target delete.
	target _ NULL.
"Architecture."
	target _ reader receiveString.
	[(String strcmp: target with: 'sun') = Int32Zero or: [(String strcmp: target with: 'intel') = Int32Zero]] whileFalse:
		[target delete.
		target _ NULL.
		writer sendString: 'no!!'.
		target _ reader receiveString].
	[((OSHandle informVM quickSearch: target) == Int32Zero)
		ifTrue: [shuffler _ SimpleShuffler create]
		ifFalse: [shuffler _ NoShuffler create]] smalltalkOnly.
	[((String strcmp: target with: 'intel') == Int32Zero)
		ifTrue: [shuffler _ SimpleShuffler create]
		ifFalse: [shuffler _ NoShuffler create]] translateOnly.
	target delete.
	target _ NULL.
	writer sendString: 'yes'.
"Syntax"
	target _ reader receiveString.
	[(String strcmp: target with: 'binary2') = Int32Zero] whileFalse:
		[target delete.
		target _ NULL.
		writer sendString: 'no!!'.
		target _ reader receiveString].
	target delete.
	target _ NULL.
	writer sendString: 'yes'.
"Semantics"
	target _ reader receiveString.
	[(String strcmp: target with: 'febe92.2') = Int32Zero] whileFalse:
		[target delete.
		target _ NULL.
		writer sendString: 'no!!'.
		target _ reader receiveString].
	writer sendString: 'yes'.
	writer destroy.
	reader destroy.
	^PromiseManager create: portal with: target with: shuffler.!
*/
}
/**
 * Create the comm detector and add it.
 */
public static void fillDetector(PromiseManager pm) {
	FeRangeElement rangeElement;
	rangeElement = (FeRangeElement) (pm.fetchNonNullHeaper(AboraSupport.findCategory(FeRangeElement.class)));
	if (pm.noErrors()) {
		FeFillDetector detector;
		detector = CommFillDetector.make(pm, pm.clientPromiseNumber(), rangeElement);
		rangeElement.addFillDetector(detector);
		pm.respondHeaper(detector);
	}
/*
udanax-top.st:35854:PromiseManager class methodsFor: 'detector requests'!
{void} fillDetector: pm {PromiseManager} 
	"Create the comm detector and add it."
	| rangeElement {FeRangeElement | NULL} |
	rangeElement _ (pm fetchNonNullHeaper: FeRangeElement) cast: FeRangeElement.
	pm noErrors ifTrue:
		[| detector {FeFillDetector} |
		detector _ CommFillDetector make: pm with: pm clientPromiseNumber with: rangeElement.
		rangeElement addFillDetector: detector.
		pm respondHeaper: detector]!
*/
}
/**
 * Create the comm detector and add it.
 */
public static void fillRangeDetector(PromiseManager pm) {
	FeEdition receiver;
	receiver = (FeEdition) (pm.fetchNonNullHeaper(AboraSupport.findCategory(FeEdition.class)));
	if (pm.noErrors()) {
		FeFillRangeDetector detector;
		detector = CommFillRangeDetector.make(pm, pm.clientPromiseNumber(), receiver);
		receiver.addFillRangeDetector(detector);
		pm.respondHeaper(detector);
	}
/*
udanax-top.st:35864:PromiseManager class methodsFor: 'detector requests'!
{void} fillRangeDetector: pm {PromiseManager} 
	"Create the comm detector and add it."
	| receiver {FeEdition | NULL} |
	receiver _ (pm fetchNonNullHeaper: FeEdition) cast: FeEdition.
	pm noErrors ifTrue:
		[| detector {FeFillRangeDetector} |
		detector _ CommFillRangeDetector make: pm with: pm clientPromiseNumber with: receiver.
		receiver addFillRangeDetector: detector.
		pm respondHeaper: detector]!
*/
}
/**
 * Create the comm detector and add it.
 */
public static void revisionDetector(PromiseManager pm) {
	FeWork receiver;
	receiver = (FeWork) (pm.fetchNonNullHeaper(AboraSupport.findCategory(FeWork.class)));
	if (pm.noErrors()) {
		FeRevisionDetector detector;
		detector = CommRevisionDetector.make(pm, pm.clientPromiseNumber(), receiver);
		receiver.addRevisionDetector(detector);
		pm.respondHeaper(detector);
	}
/*
udanax-top.st:35874:PromiseManager class methodsFor: 'detector requests'!
{void} revisionDetector: pm {PromiseManager} 
	"Create the comm detector and add it."
	| receiver {FeWork | NULL} |
	receiver _ (pm fetchNonNullHeaper: FeWork) cast: FeWork.
	pm noErrors ifTrue:
		[| detector {FeRevisionDetector} |
		detector _ CommRevisionDetector make: pm with: pm clientPromiseNumber with: receiver.
		receiver addRevisionDetector: detector.
		pm respondHeaper: detector]!
*/
}
/**
 * Create the comm detector and add it.
 */
public static void statusDetector(PromiseManager pm) {
	FeWork receiver;
	receiver = (FeWork) (pm.fetchNonNullHeaper(AboraSupport.findCategory(FeWork.class)));
	if (pm.noErrors()) {
		FeStatusDetector detector;
		detector = CommStatusDetector.make(pm, pm.clientPromiseNumber(), receiver);
		receiver.addStatusDetector(detector);
		pm.respondHeaper(detector);
	}
/*
udanax-top.st:35884:PromiseManager class methodsFor: 'detector requests'!
{void} statusDetector: pm {PromiseManager} 
	"Create the comm detector and add it."
	| receiver {FeWork | NULL} |
	receiver _ (pm fetchNonNullHeaper: FeWork) cast: FeWork.
	pm noErrors ifTrue:
		[| detector {FeStatusDetector} |
		detector _ CommStatusDetector make: pm with: pm clientPromiseNumber with: receiver.
		receiver addStatusDetector: detector.
		pm respondHeaper: detector]!
*/
}
/**
 * Create the comm detector and add it.
 */
public static void waitForConsequences(PromiseManager pm) {
	FeWaitDetector detector;
	detector = CommWaitDetector.make(pm, pm.clientPromiseNumber());
	FeServer.waitForConsequences(detector);
	pm.respondHeaper(detector);
/*
udanax-top.st:35894:PromiseManager class methodsFor: 'detector requests'!
{void} waitForConsequences: pm {PromiseManager} 
	"Create the comm detector and add it."
	| detector {FeWaitDetector} |
	detector _ CommWaitDetector make: pm with: pm clientPromiseNumber.
	FeServer waitForConsequences: detector.
	pm respondHeaper: detector!
*/
}
/**
 * Create the comm detector and add it.
 */
public static void waitForWrite(PromiseManager pm) {
	FeWaitDetector detector;
	detector = CommWaitDetector.make(pm, pm.clientPromiseNumber());
	FeServer.waitForWrite(detector);
	pm.respondHeaper(detector);
/*
udanax-top.st:35901:PromiseManager class methodsFor: 'detector requests'!
{void} waitForWrite: pm {PromiseManager} 
	"Create the comm detector and add it."
	| detector {FeWaitDetector} |
	detector _ CommWaitDetector make: pm with: pm clientPromiseNumber.
	FeServer waitForWrite: detector.
	pm respondHeaper: detector!
*/
}
public static void delayCast(PromiseManager pm) {
	Heaper result;
	Category cat;
	result = pm.fetchHeaper(AboraSupport.findCategory(Heaper.class));
	cat = pm.fetchCategory();
	if (pm.noErrors()) {
		if ( ! (result == null || (result.isKindOf(cat)))) {
			throw new AboraRuntimeException(AboraRuntimeException.CAST_FAILED);
		}
		pm.respondHeaper(result);
	}
/*
udanax-top.st:35910:PromiseManager class methodsFor: 'misc requests'!
{void} delayCast: pm {PromiseManager} 
	
	| result {Heaper} cat {Category} |
	result _ pm fetchHeaper: Heaper.
	cat _ pm fetchCategory.
	pm noErrors ifTrue:
		[(result == NULL or: [result isKindOf: cat]) 
			ifFalse: [Heaper BLAST: #CastFailed].
		pm respondHeaper: result]!
*/
}
public static void equals(PromiseManager pm) {
	Heaper me;
	Heaper him;
	me = pm.fetchNonNullHeaper(AboraSupport.findCategory(Heaper.class));
	him = pm.fetchNonNullHeaper(AboraSupport.findCategory(Heaper.class));
	if (pm.noErrors()) {
		pm.respondBooleanVar((me.isEqual(him)));
	}
/*
udanax-top.st:35920:PromiseManager class methodsFor: 'misc requests'!
{void} equals: pm {PromiseManager} 
	
	| me {Heaper} him {Heaper} |
	me _ pm fetchNonNullHeaper: Heaper.
	him _ pm fetchNonNullHeaper: Heaper.
	pm noErrors ifTrue:
		[pm respondBooleanVar: (me isEqual: him)]!
*/
}
/**
 * The zero argument version of PrimArray export.
 */
public static void export0(PromiseManager pm) {
	PrimArray array;
	array = (PrimArray) (pm.fetchNonNullHeaper(AboraSupport.findCategory(PrimArray.class)));
	if (pm.noErrors()) {
		if (array instanceof PrimIntArray) {
			PrimIntArray a = (PrimIntArray) array;
			pm.sendInts(a, a.count(), 0);
		}
		else if (array instanceof IntegerVarArray) {
			IntegerVarArray a1 = (IntegerVarArray) array;
			pm.sendHumbers(a1, a1.count(), 0);
		}
		else if (array instanceof PtrArray) {
			PtrArray a2 = (PtrArray) array;
			pm.sendPromises(a2, a2.count(), 0);
		}
		else if (array instanceof PrimFloatArray) {
			PrimFloatArray a3 = (PrimFloatArray) array;
			pm.sendIEEEs(a3, a3.count(), 0);
		}
	}
/*
udanax-top.st:35928:PromiseManager class methodsFor: 'misc requests'!
{void} export0: pm {PromiseManager} 
	"The zero argument version of PrimArray export."
	
	| array {PrimArray} |
	array _ (pm fetchNonNullHeaper: PrimArray) cast: PrimArray.
	pm noErrors ifTrue:
		[array cast: PrimIntArray into: [:a | pm sendInts: a with: a count with: Int32Zero]
			 cast: IntegerVarArray into: [:a | pm sendHumbers: a with: a count with: Int32Zero]
			 cast: PtrArray into: [:a | pm sendPromises: a with: a count with: Int32Zero]
			 cast: PrimFloatArray into: [:a | pm sendIEEEs: a with: a count with: Int32Zero]]!
*/
}
/**
 * The one argument version of PrimArray export.
 */
public static void export1(PromiseManager pm) {
	PrimArray array;
	int count;
	array = (PrimArray) (pm.fetchNonNullHeaper(AboraSupport.findCategory(PrimArray.class)));
	count = pm.fetchInt32();
	if (pm.noErrors()) {
		if (array instanceof PrimIntArray) {
			PrimIntArray a = (PrimIntArray) array;
			pm.sendInts(a, count, 0);
		}
		else if (array instanceof IntegerVarArray) {
			IntegerVarArray a1 = (IntegerVarArray) array;
			pm.sendHumbers(a1, count, 0);
		}
		else if (array instanceof PtrArray) {
			PtrArray a2 = (PtrArray) array;
			pm.sendPromises(a2, count, 0);
		}
		else if (array instanceof PrimFloatArray) {
			PrimFloatArray a3 = (PrimFloatArray) array;
			pm.sendIEEEs(a3, count, 0);
		}
	}
/*
udanax-top.st:35939:PromiseManager class methodsFor: 'misc requests'!
{void} export1: pm {PromiseManager} 
	"The one argument version of PrimArray export."
	
	| array {PrimArray} count {Int32} |
	array _ (pm fetchNonNullHeaper: PrimArray) cast: PrimArray.
	count _ pm fetchInt32.
	pm noErrors ifTrue:
		[array cast: PrimIntArray into: [:a | pm sendInts: a with: count with: Int32Zero]
			 cast: IntegerVarArray into: [:a | pm sendHumbers: a with: count with: Int32Zero]
			 cast: PtrArray into: [:a | pm sendPromises: a with: count with: Int32Zero]
			 cast: PrimFloatArray into: [:a | pm sendIEEEs: a with: count with: Int32Zero]]!
*/
}
/**
 * The two argument version of PrimArray export.
 */
public static void export2(PromiseManager pm) {
	PrimArray array;
	int count;
	int start;
	array = (PrimArray) (pm.fetchNonNullHeaper(AboraSupport.findCategory(PrimArray.class)));
	count = pm.fetchInt32();
	start = pm.fetchInt32();
	if (pm.noErrors()) {
		if (array instanceof PrimIntArray) {
			PrimIntArray a = (PrimIntArray) array;
			pm.sendInts(a, count, start);
		}
		else if (array instanceof IntegerVarArray) {
			IntegerVarArray a1 = (IntegerVarArray) array;
			pm.sendHumbers(a1, count, start);
		}
		else if (array instanceof PtrArray) {
			PtrArray a2 = (PtrArray) array;
			pm.sendPromises(a2, count, start);
		}
		else if (array instanceof PrimFloatArray) {
			PrimFloatArray a3 = (PrimFloatArray) array;
			pm.sendIEEEs(a3, count, start);
		}
	}
/*
udanax-top.st:35951:PromiseManager class methodsFor: 'misc requests'!
{void} export2: pm {PromiseManager} 
	"The two argument version of PrimArray export."
	
	| array {PrimArray} count {Int32} start {Int32} |
	array _ (pm fetchNonNullHeaper: PrimArray) cast: PrimArray.
	count _ pm fetchInt32.
	start _ pm fetchInt32.
	pm noErrors ifTrue:
		[array cast: PrimIntArray into: [:a | pm sendInts: a with: count with: start]
			 cast: IntegerVarArray into: [:a | pm sendHumbers: a with: count with: start]
			 cast: PtrArray into: [:a | pm sendPromises: a with: count with: start]
			 cast: PrimFloatArray into: [:a | pm sendIEEEs: a with: count with: start]]!
*/
}
public static void forceIt(PromiseManager pm) {
	pm.force();
/*
udanax-top.st:35964:PromiseManager class methodsFor: 'misc requests'!
{void} forceIt: pm {PromiseManager} 
	pm force!
*/
}
/**
 * PromiseManager makeRequestTable.
 */
public static PtrArray makeRequestTable() {
	PtrArray table;
	table = PtrArray.nulls(500);
	table.storeAll((SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("NO_REQUEST_", "VHFn")))));
	fillRequestTable(table);
	return table;
/*
udanax-top.st:35967:PromiseManager class methodsFor: 'misc requests'!
{PtrArray of: RequestHandler} makeRequestTable
	"PromiseManager makeRequestTable."
	| table {PtrArray} |
	table _ PtrArray nulls: 500.
	table storeAll: (SpecialHandler make: (PromiseManager pointerToStaticMember: #noRequest: with: 'VHFn')).
	self fillRequestTable: table.
	^table!
*/
}
/**
 * For illegal requests.
 */
public static void noRequest(PromiseManager pm) {
	throw new AboraRuntimeException(AboraRuntimeException.BAD_REQUEST);
/*
udanax-top.st:35975:PromiseManager class methodsFor: 'misc requests'!
{void} noRequest: pm {PromiseManager unused} 
	"For illegal requests."
	
	Heaper BLAST: #BadRequest!
*/
}
/**
 * For illegal requests.
 */
public static void notLoggedInRequest(PromiseManager pm) {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_LOGGED_IN);
/*
udanax-top.st:35980:PromiseManager class methodsFor: 'misc requests'!
{void} notLoggedInRequest: pm {PromiseManager unused} 
	"For illegal requests."
	
	Heaper BLAST: #NotLoggedIn!
*/
}
public static void promiseHash(PromiseManager pm) {
	Heaper me;
	me = pm.fetchNonNullHeaper(AboraSupport.findCategory(Heaper.class));
	if (pm.noErrors()) {
		pm.respondIntegerVar(me.hashForEqual());
	}
/*
udanax-top.st:35985:PromiseManager class methodsFor: 'misc requests'!
{void} promiseHash: pm {PromiseManager} 
	
	| me {Heaper}  |
	me _ pm fetchNonNullHeaper: Heaper.
	pm noErrors ifTrue:
		[pm respondIntegerVar: me hashForEqual]!
*/
}
/**
 * The one argument version of PrimArray export.
 */
public static void setCurrentAuthor(PromiseManager pm) {
	ID iD;
	iD = (ID) (pm.fetchHeaper(AboraSupport.findCategory(ID.class)));
	if (pm.noErrors()) {
		FeServer.setCurrentAuthor(iD);
	}
/*
udanax-top.st:35992:PromiseManager class methodsFor: 'misc requests'!
{void} setCurrentAuthor: pm {PromiseManager}
	"The one argument version of PrimArray export."
	
	| iD {ID}  |
	iD _ (pm fetchHeaper: ID) cast: ID.
	pm noErrors ifTrue:
		[FeServer setCurrentAuthor: iD]!
*/
}
/**
 * Set the fluid.
 */
public static void setCurrentKeyMaster(PromiseManager pm) {
	FeKeyMaster keymaster;
	keymaster = (FeKeyMaster) (pm.fetchHeaper(AboraSupport.findCategory(FeKeyMaster.class)));
	if (pm.noErrors()) {
		FeServer.setCurrentKeyMaster(keymaster);
	}
/*
udanax-top.st:36000:PromiseManager class methodsFor: 'misc requests'!
{void} setCurrentKeyMaster: pm {PromiseManager}
	"Set the fluid."
	
	| keymaster {FeKeyMaster}  |
	keymaster _ (pm fetchHeaper: FeKeyMaster) cast: FeKeyMaster.
	pm noErrors ifTrue:
		[FeServer setCurrentKeyMaster: keymaster]!
*/
}
/**
 * Set the fluid.
 */
public static void setInitialEditClub(PromiseManager pm) {
	ID iD;
	iD = (ID) (pm.fetchHeaper(AboraSupport.findCategory(ID.class)));
	if (pm.noErrors()) {
		FeServer.setInitialEditClub(iD);
	}
/*
udanax-top.st:36008:PromiseManager class methodsFor: 'misc requests'!
{void} setInitialEditClub: pm {PromiseManager}
	"Set the fluid."
	
	| iD {ID}  |
	iD _ (pm fetchHeaper: ID) cast: ID.
	pm noErrors ifTrue:
		[FeServer setInitialEditClub: iD]!
*/
}
/**
 * Set the fluid.
 */
public static void setInitialOwner(PromiseManager pm) {
	ID iD;
	iD = (ID) (pm.fetchHeaper(AboraSupport.findCategory(ID.class)));
	if (pm.noErrors()) {
		FeServer.setInitialOwner(iD);
	}
/*
udanax-top.st:36016:PromiseManager class methodsFor: 'misc requests'!
{void} setInitialOwner: pm {PromiseManager}
	"Set the fluid."
	
	| iD {ID}  |
	iD _ (pm fetchHeaper: ID) cast: ID.
	pm noErrors ifTrue:
		[FeServer setInitialOwner: iD]!
*/
}
/**
 * Set the fluid.
 */
public static void setInitialReadClub(PromiseManager pm) {
	ID iD;
	iD = (ID) (pm.fetchHeaper(AboraSupport.findCategory(ID.class)));
	if (pm.noErrors()) {
		FeServer.setInitialReadClub(iD);
	}
/*
udanax-top.st:36024:PromiseManager class methodsFor: 'misc requests'!
{void} setInitialReadClub: pm {PromiseManager}
	"Set the fluid."
	
	| iD {ID}  |
	iD _ (pm fetchHeaper: ID) cast: ID.
	pm noErrors ifTrue:
		[FeServer setInitialReadClub: iD]!
*/
}
/**
 * Set the fluid.
 */
public static void setInitialSponsor(PromiseManager pm) {
	ID iD;
	iD = (ID) (pm.fetchHeaper(AboraSupport.findCategory(ID.class)));
	if (pm.noErrors()) {
		FeServer.setInitialSponsor(iD);
	}
/*
udanax-top.st:36032:PromiseManager class methodsFor: 'misc requests'!
{void} setInitialSponsor: pm {PromiseManager}
	"Set the fluid."
	
	| iD {ID}  |
	iD _ (pm fetchHeaper: ID) cast: ID.
	pm noErrors ifTrue:
		[FeServer setInitialSponsor: iD]!
*/
}
public static void shutdown(PromiseManager pm) {
	FeAdminer adm;
	adm = (FeAdminer) (pm.fetchNonNullHeaper(AboraSupport.findCategory(FeAdminer.class)));
	if (pm.noErrors()) {
		pm.force();
		adm.shutdown();
	}
/*
udanax-top.st:36040:PromiseManager class methodsFor: 'misc requests'!
{void} shutdown: pm {PromiseManager}
	| adm {FeAdminer} |
	adm := (pm fetchNonNullHeaper: FeAdminer) cast: FeAdminer.
	pm noErrors ifTrue:
		[pm force.
		adm shutdown]!
*/
}
public static void testKindOf(PromiseManager pm) {
	Heaper result;
	Category cat;
	result = pm.fetchNonNullHeaper(AboraSupport.findCategory(Heaper.class));
	cat = pm.fetchCategory();
	if (pm.noErrors()) {
		pm.respondBooleanVar((result.isKindOf(cat)));
	}
/*
udanax-top.st:36048:PromiseManager class methodsFor: 'misc requests'!
{void} testKindOf: pm {PromiseManager} 
	| result {Heaper} cat {Category} |
	result _ pm fetchNonNullHeaper: Heaper.
	cat _ pm fetchCategory.
	pm noErrors ifTrue:
		[pm respondBooleanVar: (result isKindOf: cat)]!
*/
}
public static void waiveEm(PromiseManager pm) {
	pm.waiveMany();
/*
udanax-top.st:36055:PromiseManager class methodsFor: 'misc requests'!
{void} waiveEm: pm {PromiseManager} 
	pm waiveMany!
*/
}
public static void waiveIt(PromiseManager pm) {
	pm.waive();
/*
udanax-top.st:36058:PromiseManager class methodsFor: 'misc requests'!
{void} waiveIt: pm {PromiseManager} 
	pm waive!
*/
}
/**
 * <sizeof> <byte>*
 */
public static void makeFloat(PromiseManager pm) {
	int size;
	size = pm.receiveIntegerVar();
	if (size == 4) {
		AboraSupport.translateOnly();
		{
			/* IEEE32 f;
		pm->readStream ()->getBytes ((void *) &f, 4);
		pm->respondHeaper (PrimIEEE32::make (f));
		return; */
		}
	}
	else {
		if (size == 8) {
			AboraSupport.translateOnly();
			{
				/* IEEE64 f;
		pm->readStream ()->getBytes ((void *) &f, 8);
		pm->respondHeaper (PrimIEEE64::make (f));
		return; */
			}
		}
	}
	for (int i = 0; i < size; i ++ ) {
		pm.readStream().getByte();
	}
	if (pm.noErrors()) {
		throw new UnimplementedException();
	}
/*
udanax-top.st:36063:PromiseManager class methodsFor: 'making requests'!
{void} makeFloat: pm {PromiseManager} 
	"<sizeof> <byte>* "
	| size {Int32} |
	size _ pm receiveIntegerVar DOTasLong.
	size = 4 ifTrue:
		['IEEE32 f;
		pm->readStream ()->getBytes ((void *) &f, 4);
		pm->respondHeaper (PrimIEEE32::make (f));
		return;' translateOnly]
	ifFalse: [size = 8 ifTrue:
		['IEEE64 f;
		pm->readStream ()->getBytes ((void *) &f, 8);
		pm->respondHeaper (PrimIEEE64::make (f));
		return;' translateOnly]].
	size timesRepeat: [pm readStream getByte].
	pm noErrors ifTrue: [self unimplemented]!
*/
}
public static void makeFloatArray(PromiseManager pm) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:36080:PromiseManager class methodsFor: 'making requests'!
{void} makeFloatArray: pm {PromiseManager} 
	| sizeofFloat {IntegerVar} count {IntegerVar} |
	sizeofFloat _ pm receiveIntegerVar.
	count _ pm receiveIntegerVar.
	pm noErrors ifTrue: 
		[| size {Int32} buffer {UInt8 vector} result {PrimFloatArray} |
		size _ (sizeofFloat * count) DOTasLong.
		[buffer _ UInt8Array make: size] smalltalkOnly.
		'if (size > 0) {
			buffer = new UInt8[size];
		} else {
			buffer = NULL;
		}' translateOnly.
		pm readStream getBytes: buffer with: size.
		result _ ((PrimSpec iEEE: (sizeofFloat * 8) DOTasLong) arrayFromBuffer: count DOTasLong with: buffer) cast: PrimFloatArray.
		buffer ~~ NULL ifTrue: [buffer delete].
		pm respondHeaper: result]!
*/
}
public static void makeHumber(PromiseManager pm) {
	int num;
	num = pm.receiveIntegerVar();
	if (pm.noErrors()) {
		pm.respondHeaper((PrimIntValue.make(num)));
	}
/*
udanax-top.st:36098:PromiseManager class methodsFor: 'making requests'!
{void} makeHumber: pm {PromiseManager} 
	| num {IntegerVar} |
	num _ pm receiveIntegerVar.
	pm noErrors ifTrue: [pm respondHeaper: (PrimIntValue make: num)]!
*/
}
public static void makeHumberArray(PromiseManager pm) {
	int count;
	PrimIntegerArray result;
	count = pm.receiveIntegerVar();
	result = IntegerVarArray.zeros(count);
	for (int i = 0; i < count; i ++ ) {
		result.storeInteger(i, pm.receiveIntegerVar());
	}
	if (pm.noErrors()) {
		pm.respondHeaper(result);
	}
/*
udanax-top.st:36103:PromiseManager class methodsFor: 'making requests'!
{void} makeHumberArray: pm {PromiseManager} 
	| count {Int32} result {PrimIntegerArray} |
	count _ pm receiveIntegerVar DOTasLong.
	result _ IntegerVarArray zeros: count.
	Int32Zero almostTo: count do: [:i {Int32} | result at: i storeInteger: pm receiveIntegerVar].
	pm noErrors ifTrue: [pm respondHeaper: result]!
*/
}
public static void makeIntArray(PromiseManager pm) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:36110:PromiseManager class methodsFor: 'making requests'!
{void} makeIntArray: pm {PromiseManager} 
	| precision {IntegerVar} count {IntegerVar} size {Int32} buffer {UInt8 vector} bits {Int32} spec {PrimSpec} result {PrimIntegerArray} |
	precision _ pm receiveIntegerVar.
	count _ pm receiveIntegerVar.
	bits _ precision abs DOTasLong.
	size _ (precision abs // 8 * count) DOTasLong.
	[buffer _ UInt8Array make: size] smalltalkOnly.
	'if (size > 0) {
		buffer = new UInt8[size];
	} else {
		buffer = NULL;
	}' translateOnly.
	pm readStream getBytes: buffer with: size.
	precision < Int32Zero 
		ifTrue: [spec _ PrimSpec signedInteger: bits] 
		ifFalse: [spec _ PrimSpec unsignedInteger: bits].
	result _ (spec arrayFromBuffer: count DOTasLong with: buffer) cast: PrimIntegerArray.
	buffer ~~ NULL ifTrue: [buffer delete].
	pm respondHeaper: result!
*/
}
/**
 * If any of the promises is an error, then pm won't return the PtrArray.
 */
public static void makePtrArray(PromiseManager pm) {
	int count;
	PtrArray result;
	count = pm.receiveIntegerVar();
	result = PtrArray.nulls(count);
	for (int i = 0; i < count; i ++ ) {
		result.store(i, (pm.fetchHeaper(AboraSupport.findCategory(Heaper.class))));
	}
	if (pm.noErrors()) {
		pm.respondHeaper(result);
	}
/*
udanax-top.st:36131:PromiseManager class methodsFor: 'making requests'!
{void} makePtrArray: pm {PromiseManager} 
	"If any of the promises is an error, then pm won't return the PtrArray."
	| count {Int32} result {PtrArray} |
	count _ pm receiveIntegerVar DOTasLong.
	result _ PtrArray nulls: count.
	Int32Zero almostTo: count do: 
		[:i {Int32} | result at: i store: (pm fetchHeaper: Heaper)].
	pm noErrors ifTrue: [pm respondHeaper: result]!
*/
}
/**
 * The table of classes and their selectors overridden by special handlers.
 */
public static void mapOverride(Object sel, Object classx, Object argCount) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:36142:PromiseManager class methodsFor: 'smalltalk: generation'!
mapOverride: sel with: class with: argCount
	"The table of classes and their selectors overridden by special handlers."
	"PromiseManager mapOverride: 'waive' with: 'Promise' with: 0"
	
	| array |
	array _ #(
		('Promise cast 1' delayCast:)
		('Promise isKindOf 1' testKindOf:)
		('Promise waive 0' waiveIt:)
		('Promise waiveMany 1' waiveEm:)
		('Promise equals 1' equals:)
		('Promise hash 0' promiseHash:)
		('Server force 0' forceIt:)
		('Adminer shutdown 0' shutdown:)
		('Array export 0' export0:)
		('Array export 1' export1:)
		('Array export 2' export2:)
		('FloatValue import 1' makeFloat:)
		('FloatArray import 1' makeFloatArray:)
		('IntValue import 1' makeHumber:)
		('HumberArray import 1' makeHumberArray:)
		('IntArray import 1' makeIntArray:)
		('PtrArray import 1' makePtrArray:)
		('RangeElement fillDetector 0' fillDetector:)
		('Edition fillRangeDetector 0' fillRangeDetector:)
		('Work revisionDetector 0' revisionDetector:)
		('Work statusDetector 0' statusDetector:)
		('Server setCurrentAuthor 1' setCurrentAuthor:)
		('Server setCurrentKeyMaster 1' setCurrentKeyMaster:)
		('Server setInitialEditClub 1' setInitialEditClub:)
		('Server setInitialOwner 1' setInitialOwner:)
		('Server setInitialReadClub 1' setInitialReadClub:)
		('Server setInitialSponsor 1' setInitialSponsor:)
		('Server waitForConsequences 0' waitForConsequences:)
		('Server waitForWrite 0' waitForWrite:)
	).
	array ~~ OverrideArray ifTrue:
		[OverrideArray _ array.
		OverrideMap _ Dictionary fromPairs: array].
	^OverrideMap at: (String streamContents: [:pp | pp << class << ' ' << sel << ' ' << argCount])
		ifAbsent: []!
*/
}
/**
 * @deprecated
 */
public static void force(PromiseManager pm) {
	throw new PasseException();
/*
udanax-top.st:36186:PromiseManager class methodsFor: 'smalltalk: passe'!
{void} force: pm {PromiseManager} 
	self passe. "use forceIt"!
*/
}
/**
 * @deprecated
 */
public static void waive(PromiseManager pm) {
	throw new PasseException();
/*
udanax-top.st:36189:PromiseManager class methodsFor: 'smalltalk: passe'!
{void} waive: pm {PromiseManager} 
	self passe!
*/
}
public static void fillClassTable(PtrArray table) {
	table.storeValue(1, AboraSupport.findCategory(Heaper.class));
	table.storeValue(2, AboraSupport.findCategory(FeAdminer.class));
	table.storeValue(3, AboraSupport.findCategory(FeArchiver.class));
	table.storeValue(4, AboraSupport.findCategory(PrimArray.class));
	table.storeValue(5, AboraSupport.findCategory(PrimFloatArray.class));
	table.storeValue(6, AboraSupport.findCategory(IntegerVarArray.class));
	table.storeValue(7, AboraSupport.findCategory(PrimIntArray.class));
	table.storeValue(8, AboraSupport.findCategory(PtrArray.class));
	table.storeValue(9, AboraSupport.findCategory(FeBundle.class));
	table.storeValue(10, AboraSupport.findCategory(FeArrayBundle.class));
	table.storeValue(11, AboraSupport.findCategory(FeElementBundle.class));
	table.storeValue(12, AboraSupport.findCategory(FePlaceHolderBundle.class));
	table.storeValue(13, AboraSupport.findCategory(CoordinateSpace.class));
	table.storeValue(14, AboraSupport.findCategory(CrossSpace.class));
	table.storeValue(15, AboraSupport.findCategory(FilterSpace.class));
	table.storeValue(16, AboraSupport.findCategory(IDSpace.class));
	table.storeValue(17, AboraSupport.findCategory(IntegerSpace.class));
	table.storeValue(18, AboraSupport.findCategory(RealSpace.class));
	table.storeValue(19, AboraSupport.findCategory(SequenceSpace.class));
	table.storeValue(20, AboraSupport.findCategory(FeFillRangeDetector.class));
	table.storeValue(21, AboraSupport.findCategory(FeFillDetector.class));
	table.storeValue(22, AboraSupport.findCategory(FeKeyMaster.class));
	table.storeValue(23, AboraSupport.findCategory(Lock.class));
	table.storeValue(24, AboraSupport.findCategory(BooLock.class));
	table.storeValue(25, AboraSupport.findCategory(ChallengeLock.class));
	table.storeValue(26, AboraSupport.findCategory(MatchLock.class));
	table.storeValue(27, AboraSupport.findCategory(MultiLock.class));
	table.storeValue(28, AboraSupport.findCategory(WallLock.class));
	table.storeValue(29, AboraSupport.findCategory(Mapping.class));
	table.storeValue(30, AboraSupport.findCategory(CrossMapping.class));
	table.storeValue(31, AboraSupport.findCategory(IntegerMapping.class));
	table.storeValue(32, AboraSupport.findCategory(SequenceMapping.class));
	table.storeValue(33, AboraSupport.findCategory(OrderSpec.class));
	table.storeValue(34, AboraSupport.findCategory(CrossOrderSpec.class));
	table.storeValue(35, AboraSupport.findCategory(Position.class));
	table.storeValue(36, AboraSupport.findCategory(FilterPosition.class));
	table.storeValue(37, AboraSupport.findCategory(ID.class));
	table.storeValue(38, AboraSupport.findCategory(Sequence.class));
	table.storeValue(39, AboraSupport.findCategory(Tuple.class));
	table.storeValue(40, AboraSupport.findCategory(IntegerPos.class));
	table.storeValue(41, AboraSupport.findCategory(RealPos.class));
	table.storeValue(42, AboraSupport.findCategory(FeRangeElement.class));
	table.storeValue(43, AboraSupport.findCategory(FeDataHolder.class));
	table.storeValue(44, AboraSupport.findCategory(FeEdition.class));
	table.storeValue(45, AboraSupport.findCategory(FeIDHolder.class));
	table.storeValue(46, AboraSupport.findCategory(FeLabel.class));
	table.storeValue(47, AboraSupport.findCategory(FeWork.class));
	table.storeValue(48, AboraSupport.findCategory(FeClub.class));
	table.storeValue(49, AboraSupport.findCategory(FeRevisionDetector.class));
	table.storeValue(50, AboraSupport.findCategory(FeServer.class));
	table.storeValue(51, AboraSupport.findCategory(FeSession.class));
	table.storeValue(52, AboraSupport.findCategory(FeStatusDetector.class));
	table.storeValue(53, AboraSupport.findCategory(Stepper.class));
	table.storeValue(54, AboraSupport.findCategory(TableStepper.class));
	table.storeValue(55, AboraSupport.findCategory(FeWaitDetector.class));
	table.storeValue(56, AboraSupport.findCategory(FeWrapper.class));
	table.storeValue(57, AboraSupport.findCategory(FeClubDescription.class));
	table.storeValue(58, AboraSupport.findCategory(FeHyperLink.class));
	table.storeValue(59, AboraSupport.findCategory(FeHyperRef.class));
	table.storeValue(60, AboraSupport.findCategory(FeMultiRef.class));
	table.storeValue(61, AboraSupport.findCategory(FeSingleRef.class));
	table.storeValue(62, AboraSupport.findCategory(FeLockSmith.class));
	table.storeValue(63, AboraSupport.findCategory(FeBooLockSmith.class));
	table.storeValue(64, AboraSupport.findCategory(FeChallengeLockSmith.class));
	table.storeValue(65, AboraSupport.findCategory(FeMatchLockSmith.class));
	table.storeValue(66, AboraSupport.findCategory(FeMultiLockSmith.class));
	table.storeValue(67, AboraSupport.findCategory(FeWallLockSmith.class));
	table.storeValue(68, AboraSupport.findCategory(FePath.class));
	table.storeValue(69, AboraSupport.findCategory(FeSet.class));
	table.storeValue(70, AboraSupport.findCategory(FeText.class));
	table.storeValue(71, AboraSupport.findCategory(FeWrapperSpec.class));
	table.storeValue(72, AboraSupport.findCategory(XnRegion.class));
	table.storeValue(73, AboraSupport.findCategory(CrossRegion.class));
	table.storeValue(74, AboraSupport.findCategory(Filter.class));
	table.storeValue(75, AboraSupport.findCategory(IDRegion.class));
	table.storeValue(76, AboraSupport.findCategory(IntegerRegion.class));
	table.storeValue(77, AboraSupport.findCategory(RealRegion.class));
	table.storeValue(78, AboraSupport.findCategory(SequenceRegion.class));
	table.storeValue(79, AboraSupport.findCategory(PrimValue.class));
	table.storeValue(80, AboraSupport.findCategory(PrimFloatValue.class));
	table.storeValue(81, AboraSupport.findCategory(PrimIntValue.class));
/*
udanax-top.st:36194:PromiseManager class methodsFor: 'translate: generated'!
{void} fillClassTable: table {PtrArray}
	
	table at: 1 storeValue: Heaper.
	table at: 2 storeValue: FeAdminer.
	table at: 3 storeValue: FeArchiver.
	table at: 4 storeValue: PrimArray.
	table at: 5 storeValue: PrimFloatArray.
	table at: 6 storeValue: IntegerVarArray.
	table at: 7 storeValue: PrimIntArray.
	table at: 8 storeValue: PtrArray.
	table at: 9 storeValue: FeBundle.
	table at: 10 storeValue: FeArrayBundle.
	table at: 11 storeValue: FeElementBundle.
	table at: 12 storeValue: FePlaceHolderBundle.
	table at: 13 storeValue: CoordinateSpace.
	table at: 14 storeValue: CrossSpace.
	table at: 15 storeValue: FilterSpace.
	table at: 16 storeValue: IDSpace.
	table at: 17 storeValue: IntegerSpace.
	table at: 18 storeValue: RealSpace.
	table at: 19 storeValue: SequenceSpace.
	table at: 20 storeValue: FeFillRangeDetector.
	table at: 21 storeValue: FeFillDetector.
	table at: 22 storeValue: FeKeyMaster.
	table at: 23 storeValue: Lock.
	table at: 24 storeValue: BooLock.
	table at: 25 storeValue: ChallengeLock.
	table at: 26 storeValue: MatchLock.
	table at: 27 storeValue: MultiLock.
	table at: 28 storeValue: WallLock.
	table at: 29 storeValue: Mapping.
	table at: 30 storeValue: CrossMapping.
	table at: 31 storeValue: IntegerMapping.
	table at: 32 storeValue: SequenceMapping.
	table at: 33 storeValue: OrderSpec.
	table at: 34 storeValue: CrossOrderSpec.
	table at: 35 storeValue: Position.
	table at: 36 storeValue: FilterPosition.
	table at: 37 storeValue: ID.
	table at: 38 storeValue: Sequence.
	table at: 39 storeValue: Tuple.
	table at: 40 storeValue: IntegerPos.
	table at: 41 storeValue: RealPos.
	table at: 42 storeValue: FeRangeElement.
	table at: 43 storeValue: FeDataHolder.
	table at: 44 storeValue: FeEdition.
	table at: 45 storeValue: FeIDHolder.
	table at: 46 storeValue: FeLabel.
	table at: 47 storeValue: FeWork.
	table at: 48 storeValue: FeClub.
	table at: 49 storeValue: FeRevisionDetector.
	table at: 50 storeValue: FeServer.
	table at: 51 storeValue: FeSession.
	table at: 52 storeValue: FeStatusDetector.
	table at: 53 storeValue: Stepper.
	table at: 54 storeValue: TableStepper.
	table at: 55 storeValue: FeWaitDetector.
	table at: 56 storeValue: FeWrapper.
	table at: 57 storeValue: FeClubDescription.
	table at: 58 storeValue: FeHyperLink.
	table at: 59 storeValue: FeHyperRef.
	table at: 60 storeValue: FeMultiRef.
	table at: 61 storeValue: FeSingleRef.
	table at: 62 storeValue: FeLockSmith.
	table at: 63 storeValue: FeBooLockSmith.
	table at: 64 storeValue: FeChallengeLockSmith.
	table at: 65 storeValue: FeMatchLockSmith.
	table at: 66 storeValue: FeMultiLockSmith.
	table at: 67 storeValue: FeWallLockSmith.
	table at: 68 storeValue: FePath.
	table at: 69 storeValue: FeSet.
	table at: 70 storeValue: FeText.
	table at: 71 storeValue: FeWrapperSpec.
	table at: 72 storeValue: XnRegion.
	table at: 73 storeValue: CrossRegion.
	table at: 74 storeValue: Filter.
	table at: 75 storeValue: IDRegion.
	table at: 76 storeValue: IntegerRegion.
	table at: 77 storeValue: RealRegion.
	table at: 78 storeValue: SequenceRegion.
	table at: 79 storeValue: PrimValue.
	table at: 80 storeValue: PrimFloatValue.
	table at: 81 storeValue: PrimIntValue.!
*/
}
public static void fillRequestTable1(PtrArray table) {
	table.storeValue(27, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("IDSPACE_UUNIQUE_UN0", "HFn")))));
	table.storeValue(283, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDSPACE_UEXPORT_UN1_", "HHFn")), AboraSupport.findCategory(IDSpace.class))));
	table.storeValue(430, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("IDSPACE_UI_DS_FROM_SERVER_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(IDSpace.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(28, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDSPACE_UNEW_IDUN1_", "HHFn")), AboraSupport.findCategory(IDSpace.class))));
	table.storeValue(29, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("IDSPACE_UNEW_IDS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(IDSpace.class), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class IntegerSpace */
	table.storeValue(30, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("INTEGER_SPACE_UMAKE_UN0", "HFn")))));
	table.storeValue(31, (HHBHandler.make(((HHBFn) RequestHandler.pointerToStaticMember("INTEGER_SPACE_UABOVE_UN2_WITH_", "HHBFn")), AboraSupport.findCategory(IntegerPos.class))));
	table.storeValue(32, (HHBHandler.make(((HHBFn) RequestHandler.pointerToStaticMember("INTEGER_SPACE_UBELOW_UN2_WITH_", "HHBFn")), AboraSupport.findCategory(IntegerPos.class))));
	table.storeValue(33, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INTEGER_SPACE_UINTERVAL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(IntegerPos.class), AboraSupport.findCategory(IntegerPos.class))));
	table.storeValue(34, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_SPACE_UPOSITION_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(35, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_SPACE_UTRANSLATION_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class RealSpace */
	table.storeValue(284, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("REAL_SPACE_UMAKE_UN0", "HFn")))));
	table.storeValue(36, (HHHBHandler.make(((HHHBFn) RequestHandler.pointerToStaticMember("REAL_SPACE_UABOVE_UN3_WITH_WITH_", "HHHBFn")), AboraSupport.findCategory(RealSpace.class), AboraSupport.findCategory(RealPos.class))));
	table.storeValue(37, (HHHBHandler.make(((HHHBFn) RequestHandler.pointerToStaticMember("REAL_SPACE_UBELOW_UN3_WITH_WITH_", "HHHBFn")), AboraSupport.findCategory(RealSpace.class), AboraSupport.findCategory(RealPos.class))));
	table.storeValue(38, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("REAL_SPACE_UINTERVAL_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(RealSpace.class), AboraSupport.findCategory(RealPos.class), AboraSupport.findCategory(RealPos.class))));
	table.storeValue(39, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REAL_SPACE_UPOSITION_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(RealSpace.class), AboraSupport.findCategory(PrimFloatValue.class))));
	/* Requests for class SequenceSpace */
	table.storeValue(40, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UMAKE_UN0", "HFn")))));
	table.storeValue(41, (HHBHandler.make(((HHBFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UABOVE_UN2_WITH_", "HHBFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(42, (HHBHandler.make(((HHBFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UBELOW_UN2_WITH_", "HHBFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(43, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UINTERVAL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Sequence.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(285, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UMAPPING_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(286, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UMAPPING_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(44, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UPOSITION_UN1_", "HHFn")), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(45, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UPOSITION_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(287, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_SPACE_UPREFIXED_BY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Sequence.class), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class FillRangeDetector */
	/* Requests for class FillDetector */
	/* Requests for class KeyMaster */
	table.storeValue(288, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("KEY_MASTER_UACTUAL_AUTHORITY_UN1_", "HHFn")), AboraSupport.findCategory(FeKeyMaster.class))));
	table.storeValue(289, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("KEY_MASTER_UCOPY_UN1_", "HHFn")), AboraSupport.findCategory(FeKeyMaster.class))));
	table.storeValue(290, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("KEY_MASTER_UHAS_AUTHORITY_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(FeKeyMaster.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(291, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("KEY_MASTER_UINCORPORATE_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeKeyMaster.class), AboraSupport.findCategory(FeKeyMaster.class))));
	table.storeValue(292, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("KEY_MASTER_ULOGIN_AUTHORITY_UN1_", "HHFn")), AboraSupport.findCategory(FeKeyMaster.class))));
	table.storeValue(293, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("KEY_MASTER_UREMOVE_LOGINS_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeKeyMaster.class), AboraSupport.findCategory(IDRegion.class))));
	/* Requests for class Lock */
	/* Requests for class BooLock */
	table.storeValue(431, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("BOO_LOCK_UBOO_UN1_", "HHFn")), AboraSupport.findCategory(BooLock.class))));
	/* Requests for class ChallengeLock */
	table.storeValue(432, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CHALLENGE_LOCK_UCHALLENGE_UN1_", "HHFn")), AboraSupport.findCategory(ChallengeLock.class))));
	table.storeValue(433, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CHALLENGE_LOCK_URESPONSE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(ChallengeLock.class), AboraSupport.findCategory(PrimIntArray.class))));
	/* Requests for class MatchLock */
	table.storeValue(434, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MATCH_LOCK_UENCRYPTED_PASSWORD_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(MatchLock.class), AboraSupport.findCategory(PrimIntArray.class))));
	/* Requests for class MultiLock */
	table.storeValue(435, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_ULOCK_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(MultiLock.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(436, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_ULOCK_NAMES_UN1_", "HHFn")), AboraSupport.findCategory(MultiLock.class))));
	/* Requests for class WallLock */
	/* Requests for class Mapping */
	table.storeValue(46, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MAPPING_UCOMBINE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Mapping.class), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(47, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_UDOMAIN_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(294, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_UDOMAIN_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(48, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_UINVERSE_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(49, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("MAPPING_UIS_COMPLETE_UN1_", "BHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(50, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("MAPPING_UIS_IDENTITY_UN1_", "BHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(51, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MAPPING_UOF_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Mapping.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(52, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MAPPING_UOF_ALL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Mapping.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(295, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_URANGE_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(296, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_URANGE_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(53, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MAPPING_URESTRICT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Mapping.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(54, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_USIMPLER_MAPPINGS_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(55, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MAPPING_UUNRESTRICTED_UN1_", "HHFn")), AboraSupport.findCategory(Mapping.class))));
	/* Requests for class CrossMapping */
	table.storeValue(297, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_MAPPING_USUB_MAPPING_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossMapping.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(298, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_MAPPING_USUB_MAPPINGS_UN1_", "HHFn")), AboraSupport.findCategory(CrossMapping.class))));
	/* Requests for class IntegerMapping */
	table.storeValue(56, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_MAPPING_UTRANSLATION_UN1_", "HHFn")), AboraSupport.findCategory(IntegerMapping.class))));
	/* Requests for class SequenceMapping */
	table.storeValue(57, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_MAPPING_USHIFT_UN1_", "HHFn")), AboraSupport.findCategory(SequenceMapping.class))));
	table.storeValue(58, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_MAPPING_UTRANSLATION_UN1_", "HHFn")), AboraSupport.findCategory(SequenceMapping.class))));
	/* Requests for class OrderSpec */
	table.storeValue(299, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ORDER_SPEC_UCOORDINATE_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(59, (BHHHHandler.make(((BHHHFn) RequestHandler.pointerToStaticMember("ORDER_SPEC_UFOLLOWS_UN3_WITH_WITH_", "BHHHFn")), AboraSupport.findCategory(OrderSpec.class), AboraSupport.findCategory(Position.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(300, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ORDER_SPEC_UREVERSED_UN1_", "HHFn")), AboraSupport.findCategory(OrderSpec.class))));
	/* Requests for class CrossOrderSpec */
	table.storeValue(301, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_ORDER_SPEC_ULEX_ORDER_UN1_", "HHFn")), AboraSupport.findCategory(CrossOrderSpec.class))));
	table.storeValue(302, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_ORDER_SPEC_USUB_ORDER_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossOrderSpec.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(303, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_ORDER_SPEC_USUB_ORDERS_UN1_", "HHFn")), AboraSupport.findCategory(CrossOrderSpec.class))));
	/* Requests for class Position */
	table.storeValue(60, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("POSITION_UAS_REGION_UN1_", "HHFn")), AboraSupport.findCategory(Position.class))));
	table.storeValue(304, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("POSITION_UCOORDINATE_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(Position.class))));
	/* Requests for class FilterPosition */
	table.storeValue(437, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FILTER_POSITION_UBASE_REGION_UN1_", "HHFn")), AboraSupport.findCategory(FilterPosition.class))));
	/* Requests for class ID */
	table.storeValue(305, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDUIMPORT_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntArray.class))));
	table.storeValue(306, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDUEXPORT_UN1_", "HHFn")), AboraSupport.findCategory(ID.class))));
	/* Requests for class Sequence */
	table.storeValue(61, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_UFIRST_INDEX_UN1_", "HHFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(307, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_UINTEGER_AT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Sequence.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(62, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_UINTEGERS_UN1_", "HHFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(63, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("SEQUENCE_UIS_ZERO_UN1_", "BHFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(308, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_ULAST_INDEX_UN1_", "HHFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(309, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_UWITH_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(Sequence.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class Tuple */
	table.storeValue(64, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("TUPLE_UCOORDINATE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Tuple.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(65, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("TUPLE_UCOORDINATES_UN1_", "HHFn")), AboraSupport.findCategory(Tuple.class))));
	/* Requests for class Integer */
	table.storeValue(66, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_UVALUE_UN1_", "HHFn")), AboraSupport.findCategory(IntegerPos.class))));
	/* Requests for class Real */
	table.storeValue(67, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REAL_UVALUE_UN1_", "HHFn")), AboraSupport.findCategory(RealPos.class))));
	/* Requests for class RangeElement */
	table.storeValue(68, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UPLACE_HOLDER_UN0", "HFn")))));
	table.storeValue(310, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UAGAIN_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(311, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UCAN_MAKE_IDENTICAL_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(312, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("FILL_DETECTOR_", "VHFn")))));
	fillRequestTable2(table);
/*
udanax-top.st:36278:PromiseManager class methodsFor: 'translate: generated'!
{void} fillRequestTable1: table {PtrArray}
	
	table at: 27 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.unique.U.N0 with: 'HFn')).
	table at: 283 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.export.U.N1: with: 'HHFn')
			with: IDSpace).
	table at: 430 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.iDsFromServer.U.N2:with: with: 'HHHFn')
			with: IDSpace
			with: Sequence).
	table at: 28 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.newID.U.N1: with: 'HHFn')
			with: IDSpace).
	table at: 29 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.newIDs.U.N2:with: with: 'HHHFn')
			with: IDSpace
			with: PrimIntValue).
"Requests for class IntegerSpace"
	table at: 30 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #IntegerSpace.U.make.U.N0 with: 'HFn')).
	table at: 31 storeValue: 
		(HHBHandler make: (RequestHandler pointerToStaticMember: #IntegerSpace.U.above.U.N2:with: with: 'HHBFn')
			with: IntegerPos).
	table at: 32 storeValue: 
		(HHBHandler make: (RequestHandler pointerToStaticMember: #IntegerSpace.U.below.U.N2:with: with: 'HHBFn')
			with: IntegerPos).
	table at: 33 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntegerSpace.U.interval.U.N2:with: with: 'HHHFn')
			with: IntegerPos
			with: IntegerPos).
	table at: 34 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntegerSpace.U.position.U.N1: with: 'HHFn')
			with: PrimIntValue).
	table at: 35 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntegerSpace.U.translation.U.N1: with: 'HHFn')
			with: PrimIntValue).
"Requests for class RealSpace"
	table at: 284 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #RealSpace.U.make.U.N0 with: 'HFn')).
	table at: 36 storeValue: 
		(HHHBHandler make: (RequestHandler pointerToStaticMember: #RealSpace.U.above.U.N3:with:with: with: 'HHHBFn')
			with: RealSpace
			with: RealPos).
	table at: 37 storeValue: 
		(HHHBHandler make: (RequestHandler pointerToStaticMember: #RealSpace.U.below.U.N3:with:with: with: 'HHHBFn')
			with: RealSpace
			with: RealPos).
	table at: 38 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #RealSpace.U.interval.U.N3:with:with: with: 'HHHHFn')
			with: RealSpace
			with: RealPos
			with: RealPos).
	table at: 39 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #RealSpace.U.position.U.N2:with: with: 'HHHFn')
			with: RealSpace
			with: PrimFloatValue).
"Requests for class SequenceSpace"
	table at: 40 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.make.U.N0 with: 'HFn')).
	table at: 41 storeValue: 
		(HHBHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.above.U.N2:with: with: 'HHBFn')
			with: Sequence).
	table at: 42 storeValue: 
		(HHBHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.below.U.N2:with: with: 'HHBFn')
			with: Sequence).
	table at: 43 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.interval.U.N2:with: with: 'HHHFn')
			with: Sequence
			with: Sequence).
	table at: 285 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.mapping.U.N1: with: 'HHFn')
			with: PrimIntValue).
	table at: 286 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.mapping.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: Sequence).
	table at: 44 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.position.U.N1: with: 'HHFn')
			with: PrimArray).
	table at: 45 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.position.U.N2:with: with: 'HHHFn')
			with: PrimArray
			with: PrimIntValue).
	table at: 287 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SequenceSpace.U.prefixedBy.U.N2:with: with: 'HHHFn')
			with: Sequence
			with: PrimIntValue).
"Requests for class FillRangeDetector"
"Requests for class FillDetector"
"Requests for class KeyMaster"
	table at: 288 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #KeyMaster.U.actualAuthority.U.N1: with: 'HHFn')
			with: FeKeyMaster).
	table at: 289 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #KeyMaster.U.copy.U.N1: with: 'HHFn')
			with: FeKeyMaster).
	table at: 290 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #KeyMaster.U.hasAuthority.U.N2:with: with: 'BHHFn')
			with: FeKeyMaster
			with: ID).
	table at: 291 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #KeyMaster.U.incorporate.U.N2:with: with: 'VHHFn')
			with: FeKeyMaster
			with: FeKeyMaster).
	table at: 292 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #KeyMaster.U.loginAuthority.U.N1: with: 'HHFn')
			with: FeKeyMaster).
	table at: 293 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #KeyMaster.U.removeLogins.U.N2:with: with: 'VHHFn')
			with: FeKeyMaster
			with: IDRegion).
"Requests for class Lock"
"Requests for class BooLock"
	table at: 431 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #BooLock.U.boo.U.N1: with: 'HHFn')
			with: BooLock).
"Requests for class ChallengeLock"
	table at: 432 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ChallengeLock.U.challenge.U.N1: with: 'HHFn')
			with: ChallengeLock).
	table at: 433 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #ChallengeLock.U.response.U.N2:with: with: 'HHHFn')
			with: ChallengeLock
			with: PrimIntArray).
"Requests for class MatchLock"
	table at: 434 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MatchLock.U.encryptedPassword.U.N2:with: with: 'HHHFn')
			with: MatchLock
			with: PrimIntArray).
"Requests for class MultiLock"
	table at: 435 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiLock.U.lock.U.N2:with: with: 'HHHFn')
			with: MultiLock
			with: Sequence).
	table at: 436 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #MultiLock.U.lockNames.U.N1: with: 'HHFn')
			with: MultiLock).
"Requests for class WallLock"
"Requests for class Mapping"
	table at: 46 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.combine.U.N2:with: with: 'HHHFn')
			with: Mapping
			with: Mapping).
	table at: 47 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.domain.U.N1: with: 'HHFn')
			with: Mapping).
	table at: 294 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.domainSpace.U.N1: with: 'HHFn')
			with: Mapping).
	table at: 48 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.inverse.U.N1: with: 'HHFn')
			with: Mapping).
	table at: 49 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.isComplete.U.N1: with: 'BHFn')
			with: Mapping).
	table at: 50 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.isIdentity.U.N1: with: 'BHFn')
			with: Mapping).
	table at: 51 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.of.U.N2:with: with: 'HHHFn')
			with: Mapping
			with: Position).
	table at: 52 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.ofAll.U.N2:with: with: 'HHHFn')
			with: Mapping
			with: XnRegion).
	table at: 295 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.range.U.N1: with: 'HHFn')
			with: Mapping).
	table at: 296 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.rangeSpace.U.N1: with: 'HHFn')
			with: Mapping).
	table at: 53 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.restrict.U.N2:with: with: 'HHHFn')
			with: Mapping
			with: XnRegion).
	table at: 54 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.simplerMappings.U.N1: with: 'HHFn')
			with: Mapping).
	table at: 55 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Mapping.U.unrestricted.U.N1: with: 'HHFn')
			with: Mapping).
"Requests for class CrossMapping"
	table at: 297 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossMapping.U.subMapping.U.N2:with: with: 'HHHFn')
			with: CrossMapping
			with: PrimIntValue).
	table at: 298 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossMapping.U.subMappings.U.N1: with: 'HHFn')
			with: CrossMapping).
"Requests for class IntegerMapping"
	table at: 56 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntegerMapping.U.translation.U.N1: with: 'HHFn')
			with: IntegerMapping).
"Requests for class SequenceMapping"
	table at: 57 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceMapping.U.shift.U.N1: with: 'HHFn')
			with: SequenceMapping).
	table at: 58 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceMapping.U.translation.U.N1: with: 'HHFn')
			with: SequenceMapping).
"Requests for class OrderSpec"
	table at: 299 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #OrderSpec.U.coordinateSpace.U.N1: with: 'HHFn')
			with: OrderSpec).
	table at: 59 storeValue: 
		(BHHHHandler make: (RequestHandler pointerToStaticMember: #OrderSpec.U.follows.U.N3:with:with: with: 'BHHHFn')
			with: OrderSpec
			with: Position
			with: Position).
	table at: 300 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #OrderSpec.U.reversed.U.N1: with: 'HHFn')
			with: OrderSpec).
"Requests for class CrossOrderSpec"
	table at: 301 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossOrderSpec.U.lexOrder.U.N1: with: 'HHFn')
			with: CrossOrderSpec).
	table at: 302 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossOrderSpec.U.subOrder.U.N2:with: with: 'HHHFn')
			with: CrossOrderSpec
			with: PrimIntValue).
	table at: 303 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossOrderSpec.U.subOrders.U.N1: with: 'HHFn')
			with: CrossOrderSpec).
"Requests for class Position"
	table at: 60 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Position.U.asRegion.U.N1: with: 'HHFn')
			with: Position).
	table at: 304 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Position.U.coordinateSpace.U.N1: with: 'HHFn')
			with: Position).
"Requests for class FilterPosition"
	table at: 437 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #FilterPosition.U.baseRegion.U.N1: with: 'HHFn')
			with: FilterPosition).
"Requests for class ID"
	table at: 305 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ID.U.import.U.N1: with: 'HHFn')
			with: PrimIntArray).
	table at: 306 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ID.U.export.U.N1: with: 'HHFn')
			with: ID).
"Requests for class Sequence"
	table at: 61 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Sequence.U.firstIndex.U.N1: with: 'HHFn')
			with: Sequence).
	table at: 307 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Sequence.U.integerAt.U.N2:with: with: 'HHHFn')
			with: Sequence
			with: PrimIntValue).
	table at: 62 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Sequence.U.integers.U.N1: with: 'HHFn')
			with: Sequence).
	table at: 63 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Sequence.U.isZero.U.N1: with: 'BHFn')
			with: Sequence).
	table at: 308 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Sequence.U.lastIndex.U.N1: with: 'HHFn')
			with: Sequence).
	table at: 309 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Sequence.U.with.U.N3:with:with: with: 'HHHHFn')
			with: Sequence
			with: PrimIntValue
			with: PrimIntValue).
"Requests for class Tuple"
	table at: 64 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Tuple.U.coordinate.U.N2:with: with: 'HHHFn')
			with: Tuple
			with: PrimIntValue).
	table at: 65 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Tuple.U.coordinates.U.N1: with: 'HHFn')
			with: Tuple).
"Requests for class Integer"
	table at: 66 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Integer.U.value.U.N1: with: 'HHFn')
			with: IntegerPos).
"Requests for class Real"
	table at: 67 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Real.U.value.U.N1: with: 'HHFn')
			with: RealPos).
"Requests for class RangeElement"
	table at: 68 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.placeHolder.U.N0 with: 'HFn')).
	table at: 310 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.again.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 311 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.canMakeIdentical.U.N2:with: with: 'BHHFn')
			with: FeRangeElement
			with: FeRangeElement).
	table at: 312 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #fillDetector: with: 'VHFn')).
	self fillRequestTable2: table.!
*/
}
public static void fillRequestTable2(PtrArray table) {
	table.storeValue(69, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UIS_IDENTICAL_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(70, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_ULABEL_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(313, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UMAKE_IDENTICAL_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(314, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UOWNER_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(71, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_URELABELLED_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(FeLabel.class))));
	table.storeValue(315, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_USET_OWNER_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(72, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UTRANSCLUDERS_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(73, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UTRANSCLUDERS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class))));
	table.storeValue(74, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UTRANSCLUDERS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(Filter.class))));
	table.storeValue(75, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UTRANSCLUDERS_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(316, (HHHHHHHandler.make(((HHHHHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UTRANSCLUDERS_UN5_WITH_WITH_WITH_WITH_", "HHHHHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(76, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UWORKS_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(77, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UWORKS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class))));
	table.storeValue(78, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UWORKS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(317, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("RANGE_ELEMENT_UWORKS_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(FeEdition.class))));
	/* Requests for class DataHolder */
	table.storeValue(79, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("DATA_HOLDER_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(PrimValue.class))));
	table.storeValue(80, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("DATA_HOLDER_UVALUE_UN1_", "HHFn")), AboraSupport.findCategory(FeDataHolder.class))));
	/* Requests for class Edition */
	table.storeValue(81, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UEMPTY_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	table.storeValue(82, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UFROM_ALL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(83, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UFROM_ARRAY_UN1_", "HHFn")), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(84, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UFROM_ARRAY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(318, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UFROM_ARRAY_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(85, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UFROM_ONE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Position.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(86, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UPLACE_HOLDERS_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(319, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UCAN_MAKE_RANGE_IDENTICAL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(320, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UCAN_MAKE_RANGE_IDENTICAL_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(87, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UCOMBINE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(88, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UCOORDINATE_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(89, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UCOPY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(321, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UCOST_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(90, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UCOUNT_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(91, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UDOMAIN_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(92, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("EDITION_UENDORSE_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(CrossRegion.class))));
	table.storeValue(93, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UENDORSEMENTS_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(322, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("FILL_RANGE_DETECTOR_", "VHFn")))));
	table.storeValue(94, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UGET_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(95, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("EDITION_UHAS_POSITION_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(96, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("EDITION_UIS_EMPTY_UN1_", "BHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(97, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("EDITION_UIS_FINITE_UN1_", "BHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(323, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("EDITION_UIS_RANGE_IDENTICAL_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(324, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UMAKE_RANGE_IDENTICAL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(325, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UMAKE_RANGE_IDENTICAL_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(98, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UMAP_SHARED_ONTO_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(326, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UMAP_SHARED_TO_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(99, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UNOT_SHARED_WITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(100, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UNOT_SHARED_WITH_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(101, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UPOSITIONS_LABELLED_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeLabel.class))));
	table.storeValue(102, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UPOSITIONS_OF_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(327, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_OWNERS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(103, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_TRANSCLUDERS_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(104, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_TRANSCLUDERS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(105, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_TRANSCLUDERS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class))));
	table.storeValue(106, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_TRANSCLUDERS_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(Filter.class))));
	table.storeValue(107, (HHHHHHHandler.make(((HHHHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_TRANSCLUDERS_UN5_WITH_WITH_WITH_WITH_", "HHHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(328, (HHHHHHHHandler.make(((HHHHHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_TRANSCLUDERS_UN6_WITH_WITH_WITH_WITH_WITH_", "HHHHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(329, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_WORKS_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(330, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_WORKS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(331, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_WORKS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class))));
	table.storeValue(332, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_WORKS_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(333, (HHHHHHHandler.make(((HHHHHHFn) RequestHandler.pointerToStaticMember("EDITION_URANGE_WORKS_UN5_WITH_WITH_WITH_WITH_", "HHHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(108, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UREBIND_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(Position.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(109, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UREPLACE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(334, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("EDITION_URETRACT_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(CrossRegion.class))));
	table.storeValue(110, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_URETRIEVE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(111, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_URETRIEVE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(112, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_URETRIEVE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(113, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("EDITION_URETRIEVE_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(OrderSpec.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(335, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_USET_RANGE_OWNERS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(336, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_USET_RANGE_OWNERS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(ID.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(114, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_USHARED_REGION_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(115, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_USHARED_REGION_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(116, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_USHARED_WITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(117, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_USHARED_WITH_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(118, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_USTEPPER_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(119, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_USTEPPER_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(337, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_USTEPPER_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(120, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UTHE_ONE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(121, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UTRANSFORMED_BY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(Mapping.class))));
	table.storeValue(122, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("EDITION_UVISIBLE_ENDORSEMENTS_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(123, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UWITH_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(Position.class), AboraSupport.findCategory(FeRangeElement.class))));
	fillRequestTable3(table);
/*
udanax-top.st:36625:PromiseManager class methodsFor: 'translate: generated'!
{void} fillRequestTable2: table {PtrArray}
	
	table at: 69 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.isIdentical.U.N2:with: with: 'BHHFn')
			with: FeRangeElement
			with: FeRangeElement).
	table at: 70 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.label.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 313 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.makeIdentical.U.N2:with: with: 'VHHFn')
			with: FeRangeElement
			with: FeRangeElement).
	table at: 314 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.owner.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 71 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.relabelled.U.N2:with: with: 'HHHFn')
			with: FeRangeElement
			with: FeLabel).
	table at: 315 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.setOwner.U.N2:with: with: 'VHHFn')
			with: FeRangeElement
			with: ID).
	table at: 72 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.transcluders.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 73 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.transcluders.U.N2:with: with: 'HHHFn')
			with: FeRangeElement
			with: Filter).
	table at: 74 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.transcluders.U.N3:with:with: with: 'HHHHFn')
			with: FeRangeElement
			with: Filter
			with: Filter).
	table at: 75 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.transcluders.U.N4:with:with:with: with: 'HHHHHFn')
			with: FeRangeElement
			with: Filter
			with: Filter
			with: PrimIntValue).
	table at: 316 storeValue: 
		(HHHHHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.transcluders.U.N5:with:with:with:with: with: 'HHHHHHFn')
			with: FeRangeElement
			with: Filter
			with: Filter
			with: PrimIntValue
			with: FeEdition).
	table at: 76 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.works.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 77 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.works.U.N2:with: with: 'HHHFn')
			with: FeRangeElement
			with: Filter).
	table at: 78 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.works.U.N3:with:with: with: 'HHHHFn')
			with: FeRangeElement
			with: Filter
			with: PrimIntValue).
	table at: 317 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #RangeElement.U.works.U.N4:with:with:with: with: 'HHHHHFn')
			with: FeRangeElement
			with: Filter
			with: PrimIntValue
			with: FeEdition).
"Requests for class DataHolder"
	table at: 79 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #DataHolder.U.make.U.N1: with: 'HHFn')
			with: PrimValue).
	table at: 80 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #DataHolder.U.value.U.N1: with: 'HHFn')
			with: FeDataHolder).
"Requests for class Edition"
	table at: 81 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.empty.U.N1: with: 'HHFn')
			with: CoordinateSpace).
	table at: 82 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.fromAll.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: FeRangeElement).
	table at: 83 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.fromArray.U.N1: with: 'HHFn')
			with: PrimArray).
	table at: 84 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.fromArray.U.N2:with: with: 'HHHFn')
			with: PrimArray
			with: XnRegion).
	table at: 318 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.fromArray.U.N3:with:with: with: 'HHHHFn')
			with: PrimArray
			with: XnRegion
			with: OrderSpec).
	table at: 85 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.fromOne.U.N2:with: with: 'HHHFn')
			with: Position
			with: FeRangeElement).
	table at: 86 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.placeHolders.U.N1: with: 'HHFn')
			with: XnRegion).
	table at: 319 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.canMakeRangeIdentical.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 320 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.canMakeRangeIdentical.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: FeEdition
			with: XnRegion).
	table at: 87 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.combine.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 88 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.coordinateSpace.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 89 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.copy.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
	table at: 321 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.cost.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: PrimIntValue).
	table at: 90 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.count.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 91 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.domain.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 92 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.endorse.U.N2:with: with: 'VHHFn')
			with: FeEdition
			with: CrossRegion).
	table at: 93 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.endorsements.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 322 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #fillRangeDetector: with: 'VHFn')).
	table at: 94 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.get.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: Position).
	table at: 95 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.hasPosition.U.N2:with: with: 'BHHFn')
			with: FeEdition
			with: Position).
	table at: 96 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.isEmpty.U.N1: with: 'BHFn')
			with: FeEdition).
	table at: 97 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.isFinite.U.N1: with: 'BHFn')
			with: FeEdition).
	table at: 323 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.isRangeIdentical.U.N2:with: with: 'BHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 324 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.makeRangeIdentical.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 325 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.makeRangeIdentical.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: FeEdition
			with: XnRegion).
	table at: 98 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.mapSharedOnto.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 326 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.mapSharedTo.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 99 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.notSharedWith.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 100 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.notSharedWith.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: FeEdition
			with: PrimIntValue).
	table at: 101 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.positionsLabelled.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeLabel).
	table at: 102 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.positionsOf.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeRangeElement).
	table at: 327 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeOwners.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
	table at: 103 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeTranscluders.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 104 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeTranscluders.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
	table at: 105 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeTranscluders.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter).
	table at: 106 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeTranscluders.U.N4:with:with:with: with: 'HHHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter
			with: Filter).
	table at: 107 storeValue: 
		(HHHHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeTranscluders.U.N5:with:with:with:with: with: 'HHHHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter
			with: Filter
			with: PrimIntValue).
	table at: 328 storeValue: 
		(HHHHHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeTranscluders.U.N6:with:with:with:with:with: with: 'HHHHHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter
			with: Filter
			with: PrimIntValue
			with: FeEdition).
	table at: 329 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeWorks.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 330 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeWorks.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
	table at: 331 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeWorks.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter).
	table at: 332 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeWorks.U.N4:with:with:with: with: 'HHHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter
			with: PrimIntValue).
	table at: 333 storeValue: 
		(HHHHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rangeWorks.U.N5:with:with:with:with: with: 'HHHHHHFn')
			with: FeEdition
			with: XnRegion
			with: Filter
			with: PrimIntValue
			with: FeEdition).
	table at: 108 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.rebind.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: Position
			with: FeEdition).
	table at: 109 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.replace.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 334 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.retract.U.N2:with: with: 'VHHFn')
			with: FeEdition
			with: CrossRegion).
	table at: 110 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.retrieve.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 111 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.retrieve.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
	table at: 112 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.retrieve.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: XnRegion
			with: OrderSpec).
	table at: 113 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.retrieve.U.N4:with:with:with: with: 'HHHHHFn')
			with: FeEdition
			with: XnRegion
			with: OrderSpec
			with: PrimIntValue).
	table at: 335 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.setRangeOwners.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: ID).
	table at: 336 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.setRangeOwners.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: ID
			with: XnRegion).
	table at: 114 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.sharedRegion.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 115 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.sharedRegion.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: FeEdition
			with: PrimIntValue).
	table at: 116 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.sharedWith.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeEdition).
	table at: 117 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.sharedWith.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: FeEdition
			with: PrimIntValue).
	table at: 118 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.stepper.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 119 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.stepper.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
	table at: 337 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.stepper.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: XnRegion
			with: OrderSpec).
	table at: 120 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.theOne.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 121 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.transformedBy.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: Mapping).
	table at: 122 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.visibleEndorsements.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 123 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.with.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: Position
			with: FeRangeElement).
	self fillRequestTable3: table.!
*/
}
public static void fillRequestTable3(PtrArray table) {
	table.storeValue(124, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("EDITION_UWITH_ALL_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(125, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UWITHOUT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(126, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("EDITION_UWITHOUT_ALL_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(XnRegion.class))));
	/* Requests for class IDHolder */
	table.storeValue(338, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDHOLDER_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(ID.class))));
	table.storeValue(339, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDHOLDER_UI_DUN1_", "HHFn")), AboraSupport.findCategory(FeIDHolder.class))));
	/* Requests for class Label */
	table.storeValue(340, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("LABEL_UMAKE_UN0", "HFn")))));
	/* Requests for class Work */
	table.storeValue(127, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(128, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("WORK_UCAN_READ_UN1_", "BHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(129, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("WORK_UCAN_REVISE_UN1_", "BHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(130, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UEDIT_CLUB_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(131, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UEDITION_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(341, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_UENDORSE_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(CrossRegion.class))));
	table.storeValue(132, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UENDORSEMENTS_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(133, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("WORK_UGRAB_UN1_", "VHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(342, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UGRABBER_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(343, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UHISTORY_CLUB_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(134, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_ULAST_REVISION_AUTHOR_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(344, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_ULAST_REVISION_NUMBER_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(135, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_ULAST_REVISION_TIME_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(136, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UREAD_CLUB_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(137, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("WORK_URELEASE_UN1_", "VHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(345, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("WORK_UREMOVE_EDIT_CLUB_UN1_", "VHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(346, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("WORK_UREMOVE_READ_CLUB_UN1_", "VHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(138, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("WORK_UREQUEST_GRAB_UN1_", "VHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(347, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_URETRACT_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(CrossRegion.class))));
	table.storeValue(139, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_UREVISE_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(348, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("REVISION_DETECTOR_", "VHFn")))));
	table.storeValue(349, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_UREVISIONS_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(350, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_USET_EDIT_CLUB_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(351, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_USET_HISTORY_CLUB_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(352, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_USET_READ_CLUB_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(353, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_USPONSOR_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(IDRegion.class))));
	table.storeValue(354, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WORK_USPONSORS_UN1_", "HHFn")), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(355, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("STATUS_DETECTOR_", "VHFn")))));
	table.storeValue(356, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("WORK_UUNSPONSOR_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(IDRegion.class))));
	/* Requests for class Club */
	table.storeValue(357, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CLUB_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(358, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("CLUB_UREMOVE_SIGNATURE_CLUB_UN1_", "VHFn")), AboraSupport.findCategory(FeClub.class))));
	table.storeValue(359, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("CLUB_USET_SIGNATURE_CLUB_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeClub.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(360, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CLUB_USIGNATURE_CLUB_UN1_", "HHFn")), AboraSupport.findCategory(FeClub.class))));
	table.storeValue(361, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CLUB_USPONSORED_WORKS_UN1_", "HHFn")), AboraSupport.findCategory(FeClub.class))));
	table.storeValue(362, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CLUB_USPONSORED_WORKS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeClub.class), AboraSupport.findCategory(Filter.class))));
	/* Requests for class RevisionDetector */
	/* Requests for class Server */
	table.storeValue(438, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UACCESS_CLUB_IDUN0", "HFn")))));
	table.storeValue(439, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UADMIN_CLUB_IDUN0", "HFn")))));
	table.storeValue(440, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UARCHIVE_CLUB_IDUN0", "HFn")))));
	table.storeValue(363, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_UASSIGN_IDUN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(364, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SERVER_UASSIGN_IDUN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeRangeElement.class), AboraSupport.findCategory(ID.class))));
	table.storeValue(441, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UCLUB_DIRECTORY_IDUN0", "HFn")))));
	table.storeValue(365, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UCURRENT_TIME_UN0", "HFn")))));
	table.storeValue(442, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UENCRYPTER_NAME_UN0", "HFn")))));
	table.storeValue(140, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("FORCE_IT_", "VHFn")))));
	table.storeValue(141, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_UGET_UN1_", "HHFn")), AboraSupport.findCategory(ID.class))));
	table.storeValue(443, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UIDENTIFIER_UN0", "HFn")))));
	table.storeValue(142, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_UI_DOF_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(143, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_UI_DS_OF_UN1_", "HHFn")), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(144, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_UI_DS_OF_RANGE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(444, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_ULOGIN_UN1_", "HHFn")), AboraSupport.findCategory(ID.class))));
	table.storeValue(445, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SERVER_ULOGIN_BY_NAME_UN1_", "HHFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(446, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UEMPTY_CLUB_IDUN0", "HFn")))));
	table.storeValue(447, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UPUBLIC_CLUB_IDUN0", "HFn")))));
	table.storeValue(448, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SERVER_UPUBLIC_KEY_UN0", "HFn")))));
	table.storeValue(145, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SET_CURRENT_AUTHOR_", "VHFn")))));
	table.storeValue(146, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SET_CURRENT_KEY_MASTER_", "VHFn")))));
	table.storeValue(147, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SET_INITIAL_EDIT_CLUB_", "VHFn")))));
	table.storeValue(148, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SET_INITIAL_OWNER_", "VHFn")))));
	table.storeValue(149, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SET_INITIAL_READ_CLUB_", "VHFn")))));
	table.storeValue(150, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SET_INITIAL_SPONSOR_", "VHFn")))));
	table.storeValue(366, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("WAIT_FOR_CONSEQUENCES_", "VHFn")))));
	table.storeValue(367, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("WAIT_FOR_WRITE_", "VHFn")))));
	/* Requests for class Session */
	table.storeValue(449, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SESSION_UCURRENT_UN0", "HFn")))));
	table.storeValue(450, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SESSION_UCONNECT_TIME_UN1_", "HHFn")), AboraSupport.findCategory(FeSession.class))));
	table.storeValue(451, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("SESSION_UEND_SESSION_UN1_", "VHFn")), AboraSupport.findCategory(FeSession.class))));
	table.storeValue(452, (VHBHandler.make(((VHBFn) RequestHandler.pointerToStaticMember("SESSION_UEND_SESSION_UN2_WITH_", "VHBFn")), AboraSupport.findCategory(FeSession.class))));
	table.storeValue(453, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SESSION_UINITIAL_LOGIN_UN1_", "HHFn")), AboraSupport.findCategory(FeSession.class))));
	table.storeValue(454, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SESSION_UPORT_UN1_", "HHFn")), AboraSupport.findCategory(FeSession.class))));
	table.storeValue(470, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("SESSION_UIS_CONNECTED_UN1_", "BHFn")), AboraSupport.findCategory(FeSession.class))));
	/* Requests for class StatusDetector */
	/* Requests for class Stepper */
	table.storeValue(151, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("STEPPER_UAT_END_UN1_", "BHFn")), AboraSupport.findCategory(Stepper.class))));
	table.storeValue(254, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("STEPPER_UCOPY_UN1_", "HHFn")), AboraSupport.findCategory(Stepper.class))));
	table.storeValue(152, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("STEPPER_UGET_UN1_", "HHFn")), AboraSupport.findCategory(Stepper.class))));
	table.storeValue(153, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("STEPPER_USTEP_UN1_", "VHFn")), AboraSupport.findCategory(Stepper.class))));
	table.storeValue(154, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("STEPPER_USTEP_MANY_UN1_", "HHFn")), AboraSupport.findCategory(Stepper.class))));
	fillRequestTable4(table);
/*
udanax-top.st:36970:PromiseManager class methodsFor: 'translate: generated'!
{void} fillRequestTable3: table {PtrArray}
	
	table at: 124 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.withAll.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: XnRegion
			with: FeRangeElement).
	table at: 125 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.without.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: Position).
	table at: 126 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Edition.U.withoutAll.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: XnRegion).
"Requests for class IDHolder"
	table at: 338 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDHolder.U.make.U.N1: with: 'HHFn')
			with: ID).
	table at: 339 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDHolder.U.iD.U.N1: with: 'HHFn')
			with: FeIDHolder).
"Requests for class Label"
	table at: 340 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Label.U.make.U.N0 with: 'HFn')).
"Requests for class Work"
	table at: 127 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.make.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 128 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Work.U.canRead.U.N1: with: 'BHFn')
			with: FeWork).
	table at: 129 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Work.U.canRevise.U.N1: with: 'BHFn')
			with: FeWork).
	table at: 130 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.editClub.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 131 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.edition.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 341 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.endorse.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: CrossRegion).
	table at: 132 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.endorsements.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 133 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Work.U.grab.U.N1: with: 'VHFn')
			with: FeWork).
	table at: 342 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.grabber.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 343 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.historyClub.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 134 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.lastRevisionAuthor.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 344 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.lastRevisionNumber.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 135 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.lastRevisionTime.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 136 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.readClub.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 137 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Work.U.release.U.N1: with: 'VHFn')
			with: FeWork).
	table at: 345 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Work.U.removeEditClub.U.N1: with: 'VHFn')
			with: FeWork).
	table at: 346 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Work.U.removeReadClub.U.N1: with: 'VHFn')
			with: FeWork).
	table at: 138 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Work.U.requestGrab.U.N1: with: 'VHFn')
			with: FeWork).
	table at: 347 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.retract.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: CrossRegion).
	table at: 139 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.revise.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: FeEdition).
	table at: 348 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #revisionDetector: with: 'VHFn')).
	table at: 349 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.revisions.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 350 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.setEditClub.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: ID).
	table at: 351 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.setHistoryClub.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: ID).
	table at: 352 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.setReadClub.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: ID).
	table at: 353 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.sponsor.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: IDRegion).
	table at: 354 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Work.U.sponsors.U.N1: with: 'HHFn')
			with: FeWork).
	table at: 355 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #statusDetector: with: 'VHFn')).
	table at: 356 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Work.U.unsponsor.U.N2:with: with: 'VHHFn')
			with: FeWork
			with: IDRegion).
"Requests for class Club"
	table at: 357 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Club.U.make.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 358 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Club.U.removeSignatureClub.U.N1: with: 'VHFn')
			with: FeClub).
	table at: 359 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Club.U.setSignatureClub.U.N2:with: with: 'VHHFn')
			with: FeClub
			with: ID).
	table at: 360 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Club.U.signatureClub.U.N1: with: 'HHFn')
			with: FeClub).
	table at: 361 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Club.U.sponsoredWorks.U.N1: with: 'HHFn')
			with: FeClub).
	table at: 362 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Club.U.sponsoredWorks.U.N2:with: with: 'HHHFn')
			with: FeClub
			with: Filter).
"Requests for class RevisionDetector"
"Requests for class Server"
	table at: 438 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.accessClubID.U.N0 with: 'HFn')).
	table at: 439 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.adminClubID.U.N0 with: 'HFn')).
	table at: 440 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.archiveClubID.U.N0 with: 'HFn')).
	table at: 363 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.assignID.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 364 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Server.U.assignID.U.N2:with: with: 'HHHFn')
			with: FeRangeElement
			with: ID).
	table at: 441 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.clubDirectoryID.U.N0 with: 'HFn')).
	table at: 365 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.currentTime.U.N0 with: 'HFn')).
	table at: 442 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.encrypterName.U.N0 with: 'HFn')).
	table at: 140 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #forceIt: with: 'VHFn')).
	table at: 141 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.get.U.N1: with: 'HHFn')
			with: ID).
	table at: 443 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.identifier.U.N0 with: 'HFn')).
	table at: 142 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.iDOf.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 143 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.iDsOf.U.N1: with: 'HHFn')
			with: FeRangeElement).
	table at: 144 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.iDsOfRange.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 444 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.login.U.N1: with: 'HHFn')
			with: ID).
	table at: 445 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Server.U.loginByName.U.N1: with: 'HHFn')
			with: Sequence).
	table at: 446 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.emptyClubID.U.N0 with: 'HFn')).
	table at: 447 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.publicClubID.U.N0 with: 'HFn')).
	table at: 448 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Server.U.publicKey.U.N0 with: 'HFn')).
	table at: 145 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #setCurrentAuthor: with: 'VHFn')).
	table at: 146 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #setCurrentKeyMaster: with: 'VHFn')).
	table at: 147 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #setInitialEditClub: with: 'VHFn')).
	table at: 148 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #setInitialOwner: with: 'VHFn')).
	table at: 149 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #setInitialReadClub: with: 'VHFn')).
	table at: 150 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #setInitialSponsor: with: 'VHFn')).
	table at: 366 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #waitForConsequences: with: 'VHFn')).
	table at: 367 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #waitForWrite: with: 'VHFn')).
"Requests for class Session"
	table at: 449 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Session.U.current.U.N0 with: 'HFn')).
	table at: 450 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Session.U.connectTime.U.N1: with: 'HHFn')
			with: FeSession).
	table at: 451 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Session.U.endSession.U.N1: with: 'VHFn')
			with: FeSession).
	table at: 452 storeValue: 
		(VHBHandler make: (RequestHandler pointerToStaticMember: #Session.U.endSession.U.N2:with: with: 'VHBFn')
			with: FeSession).
	table at: 453 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Session.U.initialLogin.U.N1: with: 'HHFn')
			with: FeSession).
	table at: 454 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Session.U.port.U.N1: with: 'HHFn')
			with: FeSession).
	table at: 470 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Session.U.isConnected.U.N1: with: 'BHFn')
			with: FeSession).
"Requests for class StatusDetector"
"Requests for class Stepper"
	table at: 151 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.atEnd.U.N1: with: 'BHFn')
			with: Stepper).
	table at: 254 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.copy.U.N1: with: 'HHFn')
			with: Stepper).
	table at: 152 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.get.U.N1: with: 'HHFn')
			with: Stepper).
	table at: 153 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.step.U.N1: with: 'VHFn')
			with: Stepper).
	table at: 154 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.stepMany.U.N1: with: 'HHFn')
			with: Stepper).
	self fillRequestTable4: table.!
*/
}
public static void fillRequestTable4(PtrArray table) {
	table.storeValue(155, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("STEPPER_USTEP_MANY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(Stepper.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(156, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("STEPPER_UTHE_ONE_UN1_", "HHFn")), AboraSupport.findCategory(Stepper.class))));
	/* Requests for class TableStepper */
	table.storeValue(157, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("TABLE_STEPPER_UPOSITION_UN1_", "HHFn")), AboraSupport.findCategory(TableStepper.class))));
	table.storeValue(158, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("TABLE_STEPPER_USTEP_MANY_PAIRS_UN1_", "HHFn")), AboraSupport.findCategory(TableStepper.class))));
	table.storeValue(159, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("TABLE_STEPPER_USTEP_MANY_PAIRS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(TableStepper.class), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class Void */
	/* Requests for class WaitDetector */
	/* Requests for class Wrapper */
	table.storeValue(160, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WRAPPER_UEDITION_UN1_", "HHFn")), AboraSupport.findCategory(FeWrapper.class))));
	table.storeValue(368, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WRAPPER_UINNER_UN1_", "HHFn")), AboraSupport.findCategory(FeWrapper.class))));
	/* Requests for class ClubDescription */
	table.storeValue(369, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CLUB_DESCRIPTION_UMAKE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeLockSmith.class))));
	table.storeValue(370, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CLUB_DESCRIPTION_ULOCK_SMITH_UN1_", "HHFn")), AboraSupport.findCategory(FeClubDescription.class))));
	table.storeValue(371, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CLUB_DESCRIPTION_UMEMBERSHIP_UN1_", "HHFn")), AboraSupport.findCategory(FeClubDescription.class))));
	table.storeValue(372, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CLUB_DESCRIPTION_UWITH_LOCK_SMITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeClubDescription.class), AboraSupport.findCategory(FeLockSmith.class))));
	table.storeValue(373, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CLUB_DESCRIPTION_UWITH_MEMBERSHIP_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeClubDescription.class), AboraSupport.findCategory(FeSet.class))));
	/* Requests for class HyperLink */
	table.storeValue(161, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_UMAKE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeHyperRef.class), AboraSupport.findCategory(FeHyperRef.class))));
	table.storeValue(162, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_UEND_AT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeHyperLink.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(163, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_UEND_NAMES_UN1_", "HHFn")), AboraSupport.findCategory(FeHyperLink.class))));
	table.storeValue(164, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_ULINK_TYPES_UN1_", "HHFn")), AboraSupport.findCategory(FeHyperLink.class))));
	table.storeValue(165, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_UWITH_END_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeHyperLink.class), AboraSupport.findCategory(Sequence.class), AboraSupport.findCategory(FeHyperRef.class))));
	table.storeValue(374, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_UWITH_LINK_TYPES_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeHyperLink.class), AboraSupport.findCategory(FeSet.class))));
	table.storeValue(375, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("HYPER_LINK_UWITHOUT_END_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeHyperLink.class), AboraSupport.findCategory(Sequence.class))));
	/* Requests for class HyperRef */
	table.storeValue(166, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("HYPER_REF_UORIGINAL_CONTEXT_UN1_", "HHFn")), AboraSupport.findCategory(FeHyperRef.class))));
	table.storeValue(167, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("HYPER_REF_UPATH_CONTEXT_UN1_", "HHFn")), AboraSupport.findCategory(FeHyperRef.class))));
	table.storeValue(376, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("HYPER_REF_UWITH_ORIGINAL_CONTEXT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeHyperRef.class), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(377, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("HYPER_REF_UWITH_PATH_CONTEXT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeHyperRef.class), AboraSupport.findCategory(FePath.class))));
	table.storeValue(378, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("HYPER_REF_UWITH_WORK_CONTEXT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeHyperRef.class), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(168, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("HYPER_REF_UWORK_CONTEXT_UN1_", "HHFn")), AboraSupport.findCategory(FeHyperRef.class))));
	/* Requests for class MultiRef */
	table.storeValue(169, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(170, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UMAKE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PtrArray.class), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(171, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UMAKE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(PtrArray.class), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(172, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UMAKE_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(PtrArray.class), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FePath.class))));
	table.storeValue(379, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UINTERSECT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiRef.class), AboraSupport.findCategory(FeMultiRef.class))));
	table.storeValue(380, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UMINUS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiRef.class), AboraSupport.findCategory(FeMultiRef.class))));
	table.storeValue(173, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UREFS_UN1_", "HHFn")), AboraSupport.findCategory(FeMultiRef.class))));
	table.storeValue(381, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UUNION_WITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiRef.class), AboraSupport.findCategory(FeMultiRef.class))));
	table.storeValue(382, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UWITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiRef.class), AboraSupport.findCategory(FeHyperRef.class))));
	table.storeValue(383, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_REF_UWITHOUT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiRef.class), AboraSupport.findCategory(FeHyperRef.class))));
	/* Requests for class SingleRef */
	table.storeValue(174, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SINGLE_REF_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(175, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SINGLE_REF_UMAKE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(176, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("SINGLE_REF_UMAKE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FeWork.class))));
	table.storeValue(177, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("SINGLE_REF_UMAKE_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FeWork.class), AboraSupport.findCategory(FePath.class))));
	table.storeValue(178, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SINGLE_REF_UEXCERPT_UN1_", "HHFn")), AboraSupport.findCategory(FeSingleRef.class))));
	table.storeValue(384, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SINGLE_REF_UWITH_EXCERPT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSingleRef.class), AboraSupport.findCategory(FeEdition.class))));
	/* Requests for class LockSmith */
	/* Requests for class BooLockSmith */
	table.storeValue(455, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("BOO_LOCK_SMITH_UMAKE_UN0", "HFn")))));
	/* Requests for class ChallengeLockSmith */
	table.storeValue(456, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CHALLENGE_LOCK_SMITH_UMAKE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntArray.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(457, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CHALLENGE_LOCK_SMITH_UENCRYPTER_NAME_UN1_", "HHFn")), AboraSupport.findCategory(FeChallengeLockSmith.class))));
	table.storeValue(458, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CHALLENGE_LOCK_SMITH_UPUBLIC_KEY_UN1_", "HHFn")), AboraSupport.findCategory(FeChallengeLockSmith.class))));
	/* Requests for class MatchLockSmith */
	table.storeValue(459, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MATCH_LOCK_SMITH_UMAKE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntArray.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(460, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MATCH_LOCK_SMITH_USCRAMBLED_PASSWORD_UN1_", "HHFn")), AboraSupport.findCategory(FeMatchLockSmith.class))));
	table.storeValue(461, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MATCH_LOCK_SMITH_USCRAMBLER_NAME_UN1_", "HHFn")), AboraSupport.findCategory(FeMatchLockSmith.class))));
	/* Requests for class MultiLockSmith */
	table.storeValue(462, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_SMITH_UMAKE_UN0", "HFn")))));
	table.storeValue(463, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_SMITH_ULOCK_SMITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiLockSmith.class), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(464, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_SMITH_ULOCK_SMITH_NAMES_UN1_", "HHFn")), AboraSupport.findCategory(FeMultiLockSmith.class))));
	table.storeValue(465, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_SMITH_UWITH_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeMultiLockSmith.class), AboraSupport.findCategory(Sequence.class), AboraSupport.findCategory(FeLockSmith.class))));
	table.storeValue(466, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("MULTI_LOCK_SMITH_UWITHOUT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeMultiLockSmith.class), AboraSupport.findCategory(Sequence.class))));
	/* Requests for class WallLockSmith */
	table.storeValue(467, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("WALL_LOCK_SMITH_UMAKE_UN0", "HFn")))));
	/* Requests for class Path */
	table.storeValue(179, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("PATH_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(180, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("PATH_UFOLLOW_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FePath.class), AboraSupport.findCategory(FeEdition.class))));
	/* Requests for class Set */
	table.storeValue(181, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("SET_UMAKE_UN0", "HFn")))));
	table.storeValue(182, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SET_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(183, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SET_UCOUNT_UN1_", "HHFn")), AboraSupport.findCategory(FeSet.class))));
	table.storeValue(184, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("SET_UINCLUDES_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(385, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SET_UINTERSECT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeSet.class))));
	table.storeValue(386, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SET_UMINUS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeSet.class))));
	table.storeValue(185, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SET_UTHE_ONE_UN1_", "HHFn")), AboraSupport.findCategory(FeSet.class))));
	table.storeValue(387, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SET_UUNION_WITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeSet.class))));
	table.storeValue(388, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SET_UWITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeRangeElement.class))));
	table.storeValue(389, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SET_UWITHOUT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeSet.class), AboraSupport.findCategory(FeRangeElement.class))));
	/* Requests for class Text */
	table.storeValue(186, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("TEXT_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(187, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("TEXT_UCONTENTS_UN1_", "HHFn")), AboraSupport.findCategory(FeText.class))));
	table.storeValue(188, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("TEXT_UCOUNT_UN1_", "HHFn")), AboraSupport.findCategory(FeText.class))));
	table.storeValue(189, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("TEXT_UEXTRACT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeText.class), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(190, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("TEXT_UINSERT_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeText.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(FeText.class))));
	table.storeValue(191, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("TEXT_UMOVE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeText.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(192, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("TEXT_UREPLACE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeText.class), AboraSupport.findCategory(IntegerRegion.class), AboraSupport.findCategory(FeText.class))));
	/* Requests for class WrapperSpec */
	table.storeValue(193, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WRAPPER_SPEC_UGET_UN1_", "HHFn")), AboraSupport.findCategory(Sequence.class))));
	table.storeValue(194, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WRAPPER_SPEC_UFILTER_UN1_", "HHFn")), AboraSupport.findCategory(FeWrapperSpec.class))));
	table.storeValue(390, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("WRAPPER_SPEC_UNAME_UN1_", "HHFn")), AboraSupport.findCategory(FeWrapperSpec.class))));
	table.storeValue(195, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("WRAPPER_SPEC_UWRAP_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeWrapperSpec.class), AboraSupport.findCategory(FeEdition.class))));
	/* Requests for class Region */
	table.storeValue(196, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UCHOOSE_MANY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(197, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("REGION_UCHOOSE_MANY_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(198, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REGION_UCHOOSE_ONE_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	fillRequestTable5(table);
/*
udanax-top.st:37233:PromiseManager class methodsFor: 'translate: generated'!
{void} fillRequestTable4: table {PtrArray}
	
	table at: 155 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.stepMany.U.N2:with: with: 'HHHFn')
			with: Stepper
			with: PrimIntValue).
	table at: 156 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Stepper.U.theOne.U.N1: with: 'HHFn')
			with: Stepper).
"Requests for class TableStepper"
	table at: 157 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #TableStepper.U.position.U.N1: with: 'HHFn')
			with: TableStepper).
	table at: 158 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #TableStepper.U.stepManyPairs.U.N1: with: 'HHFn')
			with: TableStepper).
	table at: 159 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #TableStepper.U.stepManyPairs.U.N2:with: with: 'HHHFn')
			with: TableStepper
			with: PrimIntValue).
"Requests for class Void"
"Requests for class WaitDetector"
"Requests for class Wrapper"
	table at: 160 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Wrapper.U.edition.U.N1: with: 'HHFn')
			with: FeWrapper).
	table at: 368 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Wrapper.U.inner.U.N1: with: 'HHFn')
			with: FeWrapper).
"Requests for class ClubDescription"
	table at: 369 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #ClubDescription.U.make.U.N2:with: with: 'HHHFn')
			with: FeSet
			with: FeLockSmith).
	table at: 370 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ClubDescription.U.lockSmith.U.N1: with: 'HHFn')
			with: FeClubDescription).
	table at: 371 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ClubDescription.U.membership.U.N1: with: 'HHFn')
			with: FeClubDescription).
	table at: 372 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #ClubDescription.U.withLockSmith.U.N2:with: with: 'HHHFn')
			with: FeClubDescription
			with: FeLockSmith).
	table at: 373 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #ClubDescription.U.withMembership.U.N2:with: with: 'HHHFn')
			with: FeClubDescription
			with: FeSet).
"Requests for class HyperLink"
	table at: 161 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.make.U.N3:with:with: with: 'HHHHFn')
			with: FeSet
			with: FeHyperRef
			with: FeHyperRef).
	table at: 162 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.endAt.U.N2:with: with: 'HHHFn')
			with: FeHyperLink
			with: Sequence).
	table at: 163 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.endNames.U.N1: with: 'HHFn')
			with: FeHyperLink).
	table at: 164 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.linkTypes.U.N1: with: 'HHFn')
			with: FeHyperLink).
	table at: 165 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.withEnd.U.N3:with:with: with: 'HHHHFn')
			with: FeHyperLink
			with: Sequence
			with: FeHyperRef).
	table at: 374 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.withLinkTypes.U.N2:with: with: 'HHHFn')
			with: FeHyperLink
			with: FeSet).
	table at: 375 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #HyperLink.U.withoutEnd.U.N2:with: with: 'HHHFn')
			with: FeHyperLink
			with: Sequence).
"Requests for class HyperRef"
	table at: 166 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #HyperRef.U.originalContext.U.N1: with: 'HHFn')
			with: FeHyperRef).
	table at: 167 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #HyperRef.U.pathContext.U.N1: with: 'HHFn')
			with: FeHyperRef).
	table at: 376 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #HyperRef.U.withOriginalContext.U.N2:with: with: 'HHHFn')
			with: FeHyperRef
			with: FeWork).
	table at: 377 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #HyperRef.U.withPathContext.U.N2:with: with: 'HHHFn')
			with: FeHyperRef
			with: FePath).
	table at: 378 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #HyperRef.U.withWorkContext.U.N2:with: with: 'HHHFn')
			with: FeHyperRef
			with: FeWork).
	table at: 168 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #HyperRef.U.workContext.U.N1: with: 'HHFn')
			with: FeHyperRef).
"Requests for class MultiRef"
	table at: 169 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.make.U.N1: with: 'HHFn')
			with: PtrArray).
	table at: 170 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.make.U.N2:with: with: 'HHHFn')
			with: PtrArray
			with: FeWork).
	table at: 171 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.make.U.N3:with:with: with: 'HHHHFn')
			with: PtrArray
			with: FeWork
			with: FeWork).
	table at: 172 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.make.U.N4:with:with:with: with: 'HHHHHFn')
			with: PtrArray
			with: FeWork
			with: FeWork
			with: FePath).
	table at: 379 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.intersect.U.N2:with: with: 'HHHFn')
			with: FeMultiRef
			with: FeMultiRef).
	table at: 380 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.minus.U.N2:with: with: 'HHHFn')
			with: FeMultiRef
			with: FeMultiRef).
	table at: 173 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.refs.U.N1: with: 'HHFn')
			with: FeMultiRef).
	table at: 381 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.unionWith.U.N2:with: with: 'HHHFn')
			with: FeMultiRef
			with: FeMultiRef).
	table at: 382 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.with.U.N2:with: with: 'HHHFn')
			with: FeMultiRef
			with: FeHyperRef).
	table at: 383 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiRef.U.without.U.N2:with: with: 'HHHFn')
			with: FeMultiRef
			with: FeHyperRef).
"Requests for class SingleRef"
	table at: 174 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SingleRef.U.make.U.N1: with: 'HHFn')
			with: FeEdition).
	table at: 175 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SingleRef.U.make.U.N2:with: with: 'HHHFn')
			with: FeEdition
			with: FeWork).
	table at: 176 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #SingleRef.U.make.U.N3:with:with: with: 'HHHHFn')
			with: FeEdition
			with: FeWork
			with: FeWork).
	table at: 177 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #SingleRef.U.make.U.N4:with:with:with: with: 'HHHHHFn')
			with: FeEdition
			with: FeWork
			with: FeWork
			with: FePath).
	table at: 178 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SingleRef.U.excerpt.U.N1: with: 'HHFn')
			with: FeSingleRef).
	table at: 384 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SingleRef.U.withExcerpt.U.N2:with: with: 'HHHFn')
			with: FeSingleRef
			with: FeEdition).
"Requests for class LockSmith"
"Requests for class BooLockSmith"
	table at: 455 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #BooLockSmith.U.make.U.N0 with: 'HFn')).
"Requests for class ChallengeLockSmith"
	table at: 456 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #ChallengeLockSmith.U.make.U.N2:with: with: 'HHHFn')
			with: PrimIntArray
			with: Sequence).
	table at: 457 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ChallengeLockSmith.U.encrypterName.U.N1: with: 'HHFn')
			with: FeChallengeLockSmith).
	table at: 458 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ChallengeLockSmith.U.publicKey.U.N1: with: 'HHFn')
			with: FeChallengeLockSmith).
"Requests for class MatchLockSmith"
	table at: 459 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MatchLockSmith.U.make.U.N2:with: with: 'HHHFn')
			with: PrimIntArray
			with: Sequence).
	table at: 460 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #MatchLockSmith.U.scrambledPassword.U.N1: with: 'HHFn')
			with: FeMatchLockSmith).
	table at: 461 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #MatchLockSmith.U.scramblerName.U.N1: with: 'HHFn')
			with: FeMatchLockSmith).
"Requests for class MultiLockSmith"
	table at: 462 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #MultiLockSmith.U.make.U.N0 with: 'HFn')).
	table at: 463 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiLockSmith.U.lockSmith.U.N2:with: with: 'HHHFn')
			with: FeMultiLockSmith
			with: Sequence).
	table at: 464 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #MultiLockSmith.U.lockSmithNames.U.N1: with: 'HHFn')
			with: FeMultiLockSmith).
	table at: 465 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #MultiLockSmith.U.with.U.N3:with:with: with: 'HHHHFn')
			with: FeMultiLockSmith
			with: Sequence
			with: FeLockSmith).
	table at: 466 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #MultiLockSmith.U.without.U.N2:with: with: 'HHHFn')
			with: FeMultiLockSmith
			with: Sequence).
"Requests for class WallLockSmith"
	table at: 467 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #WallLockSmith.U.make.U.N0 with: 'HFn')).
"Requests for class Path"
	table at: 179 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Path.U.make.U.N1: with: 'HHFn')
			with: PtrArray).
	table at: 180 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Path.U.follow.U.N2:with: with: 'HHHFn')
			with: FePath
			with: FeEdition).
"Requests for class Set"
	table at: 181 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Set.U.make.U.N0 with: 'HFn')).
	table at: 182 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Set.U.make.U.N1: with: 'HHFn')
			with: PtrArray).
	table at: 183 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Set.U.count.U.N1: with: 'HHFn')
			with: FeSet).
	table at: 184 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Set.U.includes.U.N2:with: with: 'BHHFn')
			with: FeSet
			with: FeRangeElement).
	table at: 385 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Set.U.intersect.U.N2:with: with: 'HHHFn')
			with: FeSet
			with: FeSet).
	table at: 386 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Set.U.minus.U.N2:with: with: 'HHHFn')
			with: FeSet
			with: FeSet).
	table at: 185 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Set.U.theOne.U.N1: with: 'HHFn')
			with: FeSet).
	table at: 387 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Set.U.unionWith.U.N2:with: with: 'HHHFn')
			with: FeSet
			with: FeSet).
	table at: 388 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Set.U.with.U.N2:with: with: 'HHHFn')
			with: FeSet
			with: FeRangeElement).
	table at: 389 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Set.U.without.U.N2:with: with: 'HHHFn')
			with: FeSet
			with: FeRangeElement).
"Requests for class Text"
	table at: 186 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Text.U.make.U.N1: with: 'HHFn')
			with: PrimArray).
	table at: 187 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Text.U.contents.U.N1: with: 'HHFn')
			with: FeText).
	table at: 188 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Text.U.count.U.N1: with: 'HHFn')
			with: FeText).
	table at: 189 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Text.U.extract.U.N2:with: with: 'HHHFn')
			with: FeText
			with: IntegerRegion).
	table at: 190 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Text.U.insert.U.N3:with:with: with: 'HHHHFn')
			with: FeText
			with: PrimIntValue
			with: FeText).
	table at: 191 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Text.U.move.U.N3:with:with: with: 'HHHHFn')
			with: FeText
			with: PrimIntValue
			with: IntegerRegion).
	table at: 192 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Text.U.replace.U.N3:with:with: with: 'HHHHFn')
			with: FeText
			with: IntegerRegion
			with: FeText).
"Requests for class WrapperSpec"
	table at: 193 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #WrapperSpec.U.get.U.N1: with: 'HHFn')
			with: Sequence).
	table at: 194 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #WrapperSpec.U.filter.U.N1: with: 'HHFn')
			with: FeWrapperSpec).
	table at: 390 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #WrapperSpec.U.name.U.N1: with: 'HHFn')
			with: FeWrapperSpec).
	table at: 195 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #WrapperSpec.U.wrap.U.N2:with: with: 'HHHFn')
			with: FeWrapperSpec
			with: FeEdition).
"Requests for class Region"
	table at: 196 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.chooseMany.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: PrimIntValue).
	table at: 197 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.chooseMany.U.N3:with:with: with: 'HHHHFn')
			with: XnRegion
			with: PrimIntValue
			with: OrderSpec).
	table at: 198 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Region.U.chooseOne.U.N1: with: 'HHFn')
			with: XnRegion).
	self fillRequestTable5: table.!
*/
}
public static void fillRequestTable5(PtrArray table) {
	table.storeValue(199, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UCHOOSE_ONE_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(200, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REGION_UCOMPLEMENT_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(201, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REGION_UCOORDINATE_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(202, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REGION_UCOUNT_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(203, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("REGION_UHAS_MEMBER_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(204, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UINTERSECT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(205, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("REGION_UINTERSECTS_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(206, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("REGION_UIS_EMPTY_UN1_", "BHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(207, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("REGION_UIS_FINITE_UN1_", "BHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(208, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("REGION_UIS_FULL_UN1_", "BHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(209, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("REGION_UIS_SUBSET_OF_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(210, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UMINUS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(211, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REGION_USTEPPER_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(212, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_USTEPPER_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(213, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REGION_UTHE_ONE_UN1_", "HHFn")), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(214, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UUNION_WITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(215, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UWITH_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Position.class))));
	table.storeValue(216, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REGION_UWITHOUT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(XnRegion.class), AboraSupport.findCategory(Position.class))));
	/* Requests for class CrossRegion */
	table.storeValue(217, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_REGION_UBOXES_UN1_", "HHFn")), AboraSupport.findCategory(CrossRegion.class))));
	table.storeValue(218, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("CROSS_REGION_UIS_BOX_UN1_", "BHFn")), AboraSupport.findCategory(CrossRegion.class))));
	table.storeValue(219, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_REGION_UPROJECTION_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossRegion.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(220, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_REGION_UPROJECTIONS_UN1_", "HHFn")), AboraSupport.findCategory(CrossRegion.class))));
	/* Requests for class Filter */
	table.storeValue(391, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FILTER_UBASE_REGION_UN1_", "HHFn")), AboraSupport.findCategory(Filter.class))));
	table.storeValue(392, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FILTER_UINTERSECTED_FILTERS_UN1_", "HHFn")), AboraSupport.findCategory(Filter.class))));
	table.storeValue(393, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("FILTER_UIS_ALL_FILTER_UN1_", "BHFn")), AboraSupport.findCategory(Filter.class))));
	table.storeValue(394, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("FILTER_UIS_ANY_FILTER_UN1_", "BHFn")), AboraSupport.findCategory(Filter.class))));
	table.storeValue(221, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("FILTER_UMATCH_UN2_WITH_", "BHHFn")), AboraSupport.findCategory(Filter.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(395, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FILTER_UUNIONED_FILTERS_UN1_", "HHFn")), AboraSupport.findCategory(Filter.class))));
	/* Requests for class IDRegion */
	table.storeValue(396, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDREGION_UIMPORT_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntArray.class))));
	table.storeValue(397, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDREGION_UEXPORT_UN1_", "HHFn")), AboraSupport.findCategory(IDRegion.class))));
	/* Requests for class IntegerRegion */
	table.storeValue(222, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_UINTERVALS_UN1_", "HHFn")), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(398, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_UINTERVALS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(IntegerRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(223, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_UIS_BOUNDED_ABOVE_UN1_", "BHFn")), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(224, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_UIS_BOUNDED_BELOW_UN1_", "BHFn")), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(225, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_UIS_INTERVAL_UN1_", "BHFn")), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(226, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_USTART_UN1_", "HHFn")), AboraSupport.findCategory(IntegerRegion.class))));
	table.storeValue(227, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INTEGER_REGION_USTOP_UN1_", "HHFn")), AboraSupport.findCategory(IntegerRegion.class))));
	/* Requests for class RealRegion */
	table.storeValue(228, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REAL_REGION_UINTERVALS_UN1_", "HHFn")), AboraSupport.findCategory(RealRegion.class))));
	table.storeValue(399, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("REAL_REGION_UINTERVALS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(RealRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(229, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("REAL_REGION_UIS_BOUNDED_ABOVE_UN1_", "BHFn")), AboraSupport.findCategory(RealRegion.class))));
	table.storeValue(230, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("REAL_REGION_UIS_BOUNDED_BELOW_UN1_", "BHFn")), AboraSupport.findCategory(RealRegion.class))));
	table.storeValue(231, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("REAL_REGION_UIS_INTERVAL_UN1_", "BHFn")), AboraSupport.findCategory(RealRegion.class))));
	table.storeValue(232, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REAL_REGION_ULOWER_BOUND_UN1_", "HHFn")), AboraSupport.findCategory(RealRegion.class))));
	table.storeValue(233, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("REAL_REGION_UUPPER_BOUND_UN1_", "HHFn")), AboraSupport.findCategory(RealRegion.class))));
	/* Requests for class SequenceRegion */
	table.storeValue(234, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UINTERVALS_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(400, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UINTERVALS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(SequenceRegion.class), AboraSupport.findCategory(OrderSpec.class))));
	table.storeValue(235, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UIS_BOUNDED_ABOVE_UN1_", "BHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(236, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UIS_BOUNDED_BELOW_UN1_", "BHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(237, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UIS_INTERVAL_UN1_", "BHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(238, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_ULOWER_EDGE_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(239, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_ULOWER_EDGE_PREFIX_LIMIT_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(240, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_ULOWER_EDGE_TYPE_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(241, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UUPPER_EDGE_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(242, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UUPPER_EDGE_PREFIX_LIMIT_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	table.storeValue(243, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("SEQUENCE_REGION_UUPPER_EDGE_TYPE_UN1_", "HHFn")), AboraSupport.findCategory(SequenceRegion.class))));
	/* Requests for class Value */
	/* Requests for class FloatValue */
	table.storeValue(244, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("MAKE_FLOAT_", "VHFn")))));
	table.storeValue(401, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FLOAT_VALUE_UBIT_COUNT_UN1_", "HHFn")), AboraSupport.findCategory(PrimFloatValue.class))));
	/* Requests for class IntValue */
	table.storeValue(245, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("MAKE_HUMBER_", "VHFn")))));
	table.storeValue(402, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UBITWISE_AND_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(403, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UBITWISE_OR_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(404, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UBITWISE_XOR_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(246, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UDIVIDED_BY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(247, (BHHHandler.make(((BHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UIS_GEUN2_WITH_", "BHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(405, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_ULEFT_SHIFT_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(248, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UMAXIMUM_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(249, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UMINIMUM_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(250, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UMINUS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(406, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UMOD_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(251, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UPLUS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(407, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UBIT_COUNT_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(252, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_VALUE_UTIMES_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
/*
udanax-top.st:37590:PromiseManager class methodsFor: 'translate: generated'!
{void} fillRequestTable5: table {PtrArray}
	
	table at: 199 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.chooseOne.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: OrderSpec).
	table at: 200 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Region.U.complement.U.N1: with: 'HHFn')
			with: XnRegion).
	table at: 201 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Region.U.coordinateSpace.U.N1: with: 'HHFn')
			with: XnRegion).
	table at: 202 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Region.U.count.U.N1: with: 'HHFn')
			with: XnRegion).
	table at: 203 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.hasMember.U.N2:with: with: 'BHHFn')
			with: XnRegion
			with: Position).
	table at: 204 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.intersect.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: XnRegion).
	table at: 205 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.intersects.U.N2:with: with: 'BHHFn')
			with: XnRegion
			with: XnRegion).
	table at: 206 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Region.U.isEmpty.U.N1: with: 'BHFn')
			with: XnRegion).
	table at: 207 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Region.U.isFinite.U.N1: with: 'BHFn')
			with: XnRegion).
	table at: 208 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Region.U.isFull.U.N1: with: 'BHFn')
			with: XnRegion).
	table at: 209 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.isSubsetOf.U.N2:with: with: 'BHHFn')
			with: XnRegion
			with: XnRegion).
	table at: 210 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.minus.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: XnRegion).
	table at: 211 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Region.U.stepper.U.N1: with: 'HHFn')
			with: XnRegion).
	table at: 212 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.stepper.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: OrderSpec).
	table at: 213 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Region.U.theOne.U.N1: with: 'HHFn')
			with: XnRegion).
	table at: 214 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.unionWith.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: XnRegion).
	table at: 215 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.with.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: Position).
	table at: 216 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Region.U.without.U.N2:with: with: 'HHHFn')
			with: XnRegion
			with: Position).
"Requests for class CrossRegion"
	table at: 217 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossRegion.U.boxes.U.N1: with: 'HHFn')
			with: CrossRegion).
	table at: 218 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #CrossRegion.U.isBox.U.N1: with: 'BHFn')
			with: CrossRegion).
	table at: 219 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossRegion.U.projection.U.N2:with: with: 'HHHFn')
			with: CrossRegion
			with: PrimIntValue).
	table at: 220 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossRegion.U.projections.U.N1: with: 'HHFn')
			with: CrossRegion).
"Requests for class Filter"
	table at: 391 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Filter.U.baseRegion.U.N1: with: 'HHFn')
			with: Filter).
	table at: 392 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Filter.U.intersectedFilters.U.N1: with: 'HHFn')
			with: Filter).
	table at: 393 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Filter.U.isAllFilter.U.N1: with: 'BHFn')
			with: Filter).
	table at: 394 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Filter.U.isAnyFilter.U.N1: with: 'BHFn')
			with: Filter).
	table at: 221 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #Filter.U.match.U.N2:with: with: 'BHHFn')
			with: Filter
			with: XnRegion).
	table at: 395 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Filter.U.unionedFilters.U.N1: with: 'HHFn')
			with: Filter).
"Requests for class IDRegion"
	table at: 396 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDRegion.U.import.U.N1: with: 'HHFn')
			with: PrimIntArray).
	table at: 397 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDRegion.U.export.U.N1: with: 'HHFn')
			with: IDRegion).
"Requests for class IntegerRegion"
	table at: 222 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.intervals.U.N1: with: 'HHFn')
			with: IntegerRegion).
	table at: 398 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.intervals.U.N2:with: with: 'HHHFn')
			with: IntegerRegion
			with: OrderSpec).
	table at: 223 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.isBoundedAbove.U.N1: with: 'BHFn')
			with: IntegerRegion).
	table at: 224 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.isBoundedBelow.U.N1: with: 'BHFn')
			with: IntegerRegion).
	table at: 225 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.isInterval.U.N1: with: 'BHFn')
			with: IntegerRegion).
	table at: 226 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.start.U.N1: with: 'HHFn')
			with: IntegerRegion).
	table at: 227 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntegerRegion.U.stop.U.N1: with: 'HHFn')
			with: IntegerRegion).
"Requests for class RealRegion"
	table at: 228 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.intervals.U.N1: with: 'HHFn')
			with: RealRegion).
	table at: 399 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.intervals.U.N2:with: with: 'HHHFn')
			with: RealRegion
			with: OrderSpec).
	table at: 229 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.isBoundedAbove.U.N1: with: 'BHFn')
			with: RealRegion).
	table at: 230 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.isBoundedBelow.U.N1: with: 'BHFn')
			with: RealRegion).
	table at: 231 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.isInterval.U.N1: with: 'BHFn')
			with: RealRegion).
	table at: 232 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.lowerBound.U.N1: with: 'HHFn')
			with: RealRegion).
	table at: 233 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #RealRegion.U.upperBound.U.N1: with: 'HHFn')
			with: RealRegion).
"Requests for class SequenceRegion"
	table at: 234 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.intervals.U.N1: with: 'HHFn')
			with: SequenceRegion).
	table at: 400 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.intervals.U.N2:with: with: 'HHHFn')
			with: SequenceRegion
			with: OrderSpec).
	table at: 235 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.isBoundedAbove.U.N1: with: 'BHFn')
			with: SequenceRegion).
	table at: 236 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.isBoundedBelow.U.N1: with: 'BHFn')
			with: SequenceRegion).
	table at: 237 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.isInterval.U.N1: with: 'BHFn')
			with: SequenceRegion).
	table at: 238 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.lowerEdge.U.N1: with: 'HHFn')
			with: SequenceRegion).
	table at: 239 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.lowerEdgePrefixLimit.U.N1: with: 'HHFn')
			with: SequenceRegion).
	table at: 240 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.lowerEdgeType.U.N1: with: 'HHFn')
			with: SequenceRegion).
	table at: 241 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.upperEdge.U.N1: with: 'HHFn')
			with: SequenceRegion).
	table at: 242 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.upperEdgePrefixLimit.U.N1: with: 'HHFn')
			with: SequenceRegion).
	table at: 243 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #SequenceRegion.U.upperEdgeType.U.N1: with: 'HHFn')
			with: SequenceRegion).
"Requests for class Value"
"Requests for class FloatValue"
	table at: 244 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #makeFloat: with: 'VHFn')).
	table at: 401 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #FloatValue.U.bitCount.U.N1: with: 'HHFn')
			with: PrimFloatValue).
"Requests for class IntValue"
	table at: 245 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #makeHumber: with: 'VHFn')).
	table at: 402 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.bitwiseAnd.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 403 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.bitwiseOr.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 404 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.bitwiseXor.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 246 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.dividedBy.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 247 storeValue: 
		(BHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.isGE.U.N2:with: with: 'BHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 405 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.leftShift.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 248 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.maximum.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 249 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.minimum.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 250 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.minus.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 406 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.mod.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 251 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.plus.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 407 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.bitCount.U.N1: with: 'HHFn')
			with: PrimIntValue).
	table at: 252 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntValue.U.times.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).!
*/
}
/**
 * Requests for class Promise
 */
public static void fillRequestTable(PtrArray table) {
	table.storeValue(253, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("WAIVE_EM_", "VHFn")))));
	table.storeValue(1, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("DELAY_CAST_", "VHFn")))));
	table.storeValue(2, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("EQUALS_", "VHFn")))));
	table.storeValue(3, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("PROMISE_HASH_", "VHFn")))));
	table.storeValue(4, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("TEST_KIND_OF_", "VHFn")))));
	table.storeValue(5, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("WAIVE_IT_", "VHFn")))));
	/* Requests for class Adminer */
	table.storeValue(408, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("ADMINER_UMAKE_UN0", "HFn")))));
	table.storeValue(409, (VHBHandler.make(((VHBFn) RequestHandler.pointerToStaticMember("ADMINER_UACCEPT_CONNECTIONS_UN2_WITH_", "VHBFn")), AboraSupport.findCategory(FeAdminer.class))));
	table.storeValue(410, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ADMINER_UACTIVE_SESSIONS_UN1_", "HHFn")), AboraSupport.findCategory(FeAdminer.class))));
	table.storeValue(411, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("ADMINER_UEXECUTE_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeAdminer.class), AboraSupport.findCategory(PrimIntArray.class))));
	table.storeValue(412, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ADMINER_UGATE_LOCK_SMITH_UN1_", "HHFn")), AboraSupport.findCategory(FeAdminer.class))));
	table.storeValue(413, (VHHHHandler.make(((VHHHFn) RequestHandler.pointerToStaticMember("ADMINER_UGRANT_UN3_WITH_WITH_", "VHHHFn")), AboraSupport.findCategory(FeAdminer.class), AboraSupport.findCategory(ID.class), AboraSupport.findCategory(IDRegion.class))));
	table.storeValue(414, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ADMINER_UGRANTS_UN1_", "HHFn")), AboraSupport.findCategory(FeAdminer.class))));
	table.storeValue(415, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("ADMINER_UGRANTS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FeAdminer.class), AboraSupport.findCategory(IDRegion.class))));
	table.storeValue(416, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("ADMINER_UGRANTS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeAdminer.class), AboraSupport.findCategory(IDRegion.class), AboraSupport.findCategory(IDRegion.class))));
	table.storeValue(417, (BHHandler.make(((BHFn) RequestHandler.pointerToStaticMember("ADMINER_UIS_ACCEPTING_CONNECTIONS_UN1_", "BHFn")), AboraSupport.findCategory(FeAdminer.class))));
	table.storeValue(418, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("ADMINER_USET_GATE_LOCK_SMITH_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeAdminer.class), AboraSupport.findCategory(FeLockSmith.class))));
	table.storeValue(419, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("SHUTDOWN_", "VHFn")))));
	/* Requests for class Archiver */
	table.storeValue(420, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("ARCHIVER_UMAKE_UN0", "HFn")))));
	table.storeValue(421, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("ARCHIVER_UARCHIVE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeArchiver.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(422, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("ARCHIVER_UMARK_ARCHIVED_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(FeArchiver.class), AboraSupport.findCategory(FeEdition.class))));
	table.storeValue(423, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("ARCHIVER_URESTORE_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(FeArchiver.class), AboraSupport.findCategory(FeEdition.class), AboraSupport.findCategory(FeEdition.class))));
	/* Requests for class Array */
	table.storeValue(468, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ARRAY_UCOPY_UN1_", "HHFn")), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(469, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("ARRAY_UCOPY_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(255, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("ARRAY_UCOPY_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(424, (HHHHHHandler.make(((HHHHHFn) RequestHandler.pointerToStaticMember("ARRAY_UCOPY_UN4_WITH_WITH_WITH_", "HHHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(425, (HHHHHHHandler.make(((HHHHHHFn) RequestHandler.pointerToStaticMember("ARRAY_UCOPY_UN5_WITH_WITH_WITH_WITH_", "HHHHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(6, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ARRAY_UCOUNT_UN1_", "HHFn")), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(7, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("EXPORT0_", "VHFn")))));
	table.storeValue(256, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("EXPORT1_", "VHFn")))));
	table.storeValue(257, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("EXPORT2_", "VHFn")))));
	table.storeValue(8, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("ARRAY_UGET_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(9, (VHHHHandler.make(((VHHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_UN3_WITH_WITH_", "VHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(Heaper.class))));
	table.storeValue(258, (VHHandler.make(((VHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_ALL_UN1_", "VHFn")), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(259, (VHHHandler.make(((VHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_ALL_UN2_WITH_", "VHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(Heaper.class))));
	table.storeValue(260, (VHHHHandler.make(((VHHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_ALL_UN3_WITH_WITH_", "VHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(Heaper.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(261, (VHHHHHandler.make(((VHHHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_ALL_UN4_WITH_WITH_WITH_", "VHHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(Heaper.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(262, (VHHHHandler.make(((VHHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_MANY_UN3_WITH_WITH_", "VHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimArray.class))));
	table.storeValue(263, (VHHHHHandler.make(((VHHHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_MANY_UN4_WITH_WITH_WITH_", "VHHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(264, (VHHHHHHandler.make(((VHHHHHFn) RequestHandler.pointerToStaticMember("ARRAY_USTORE_MANY_UN5_WITH_WITH_WITH_WITH_", "VHHHHHFn")), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimArray.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class FloatArray */
	table.storeValue(265, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("MAKE_FLOAT_ARRAY_", "VHFn")))));
	table.storeValue(266, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("FLOAT_ARRAY_UZEROS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(267, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FLOAT_ARRAY_UBIT_COUNT_UN1_", "HHFn")), AboraSupport.findCategory(PrimFloatArray.class))));
	/* Requests for class HumberArray */
	table.storeValue(268, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("MAKE_HUMBER_ARRAY_", "VHFn")))));
	table.storeValue(269, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("HUMBER_ARRAY_UZEROS_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class IntArray */
	table.storeValue(10, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("MAKE_INT_ARRAY_", "VHFn")))));
	table.storeValue(11, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("INT_ARRAY_UZEROS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(12, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("INT_ARRAY_UBIT_COUNT_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntArray.class))));
	/* Requests for class PtrArray */
	table.storeValue(270, (SpecialHandler.make(((VHFn) PromiseManager.pointerToStaticMember("MAKE_PTR_ARRAY_", "VHFn")))));
	table.storeValue(271, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("PTR_ARRAY_UNULLS_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntValue.class))));
	/* Requests for class Bundle */
	table.storeValue(13, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("BUNDLE_UREGION_UN1_", "HHFn")), AboraSupport.findCategory(FeBundle.class))));
	/* Requests for class ArrayBundle */
	table.storeValue(14, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ARRAY_BUNDLE_UARRAY_UN1_", "HHFn")), AboraSupport.findCategory(FeArrayBundle.class))));
	table.storeValue(15, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ARRAY_BUNDLE_UORDERING_UN1_", "HHFn")), AboraSupport.findCategory(FeArrayBundle.class))));
	/* Requests for class ElementBundle */
	table.storeValue(16, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("ELEMENT_BUNDLE_UELEMENT_UN1_", "HHFn")), AboraSupport.findCategory(FeElementBundle.class))));
	/* Requests for class PlaceHolderBundle */
	/* Requests for class CoordinateSpace */
	table.storeValue(272, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("COORDINATE_SPACE_UASCENDING_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	table.storeValue(273, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("COORDINATE_SPACE_UCOMPLETE_MAPPING_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CoordinateSpace.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(274, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("COORDINATE_SPACE_UDESCENDING_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	table.storeValue(17, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("COORDINATE_SPACE_UEMPTY_REGION_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	table.storeValue(18, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("COORDINATE_SPACE_UFULL_REGION_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	table.storeValue(19, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("COORDINATE_SPACE_UIDENTITY_MAPPING_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	/* Requests for class CrossSpace */
	table.storeValue(20, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(275, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UAXES_UN1_", "HHFn")), AboraSupport.findCategory(CrossSpace.class))));
	table.storeValue(276, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UAXIS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PrimIntValue.class))));
	table.storeValue(277, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UAXIS_COUNT_UN1_", "HHFn")), AboraSupport.findCategory(CrossSpace.class))));
	table.storeValue(278, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_MAPPINGS_UN1_", "HHFn")), AboraSupport.findCategory(CrossSpace.class))));
	table.storeValue(279, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_MAPPINGS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(426, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_ORDER_SPECS_UN1_", "HHFn")), AboraSupport.findCategory(CrossSpace.class))));
	table.storeValue(427, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_ORDER_SPECS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(428, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_ORDER_SPECS_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PtrArray.class), AboraSupport.findCategory(PrimIntArray.class))));
	table.storeValue(21, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_POSITIONS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(22, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UCROSS_OF_REGIONS_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PtrArray.class))));
	table.storeValue(23, (HHHHHandler.make(((HHHHFn) RequestHandler.pointerToStaticMember("CROSS_SPACE_UEXTRUSION_UN3_WITH_WITH_", "HHHHFn")), AboraSupport.findCategory(CrossSpace.class), AboraSupport.findCategory(PrimIntValue.class), AboraSupport.findCategory(XnRegion.class))));
	/* Requests for class FilterSpace */
	table.storeValue(280, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FILTER_SPACE_UMAKE_UN1_", "HHFn")), AboraSupport.findCategory(CoordinateSpace.class))));
	table.storeValue(24, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("FILTER_SPACE_UALL_FILTER_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FilterSpace.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(25, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("FILTER_SPACE_UANY_FILTER_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FilterSpace.class), AboraSupport.findCategory(XnRegion.class))));
	table.storeValue(281, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("FILTER_SPACE_UBASE_SPACE_UN1_", "HHFn")), AboraSupport.findCategory(FilterSpace.class))));
	table.storeValue(429, (HHHHandler.make(((HHHFn) RequestHandler.pointerToStaticMember("FILTER_SPACE_UPOSITION_UN2_WITH_", "HHHFn")), AboraSupport.findCategory(FilterSpace.class), AboraSupport.findCategory(XnRegion.class))));
	/* Requests for class IDSpace */
	table.storeValue(26, (HHandler.make(((HFn) RequestHandler.pointerToStaticMember("IDSPACE_UGLOBAL_UN0", "HFn")))));
	table.storeValue(282, (HHHandler.make(((HHFn) RequestHandler.pointerToStaticMember("IDSPACE_UIMPORT_UN1_", "HHFn")), AboraSupport.findCategory(PrimIntArray.class))));
	fillRequestTable1(table);
/*
udanax-top.st:37858:PromiseManager class methodsFor: 'translate: generated'!
{void} fillRequestTable: table {PtrArray}
	
"Requests for class Promise"
	table at: 253 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #waiveEm: with: 'VHFn')).
	table at: 1 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #delayCast: with: 'VHFn')).
	table at: 2 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #equals: with: 'VHFn')).
	table at: 3 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #promiseHash: with: 'VHFn')).
	table at: 4 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #testKindOf: with: 'VHFn')).
	table at: 5 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #waiveIt: with: 'VHFn')).
"Requests for class Adminer"
	table at: 408 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.make.U.N0 with: 'HFn')).
	table at: 409 storeValue: 
		(VHBHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.acceptConnections.U.N2:with: with: 'VHBFn')
			with: FeAdminer).
	table at: 410 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.activeSessions.U.N1: with: 'HHFn')
			with: FeAdminer).
	table at: 411 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.execute.U.N2:with: with: 'VHHFn')
			with: FeAdminer
			with: PrimIntArray).
	table at: 412 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.gateLockSmith.U.N1: with: 'HHFn')
			with: FeAdminer).
	table at: 413 storeValue: 
		(VHHHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.grant.U.N3:with:with: with: 'VHHHFn')
			with: FeAdminer
			with: ID
			with: IDRegion).
	table at: 414 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.grants.U.N1: with: 'HHFn')
			with: FeAdminer).
	table at: 415 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.grants.U.N2:with: with: 'HHHFn')
			with: FeAdminer
			with: IDRegion).
	table at: 416 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.grants.U.N3:with:with: with: 'HHHHFn')
			with: FeAdminer
			with: IDRegion
			with: IDRegion).
	table at: 417 storeValue: 
		(BHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.isAcceptingConnections.U.N1: with: 'BHFn')
			with: FeAdminer).
	table at: 418 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Adminer.U.setGateLockSmith.U.N2:with: with: 'VHHFn')
			with: FeAdminer
			with: FeLockSmith).
	table at: 419 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #shutdown: with: 'VHFn')).
"Requests for class Archiver"
	table at: 420 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #Archiver.U.make.U.N0 with: 'HFn')).
	table at: 421 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Archiver.U.archive.U.N3:with:with: with: 'HHHHFn')
			with: FeArchiver
			with: FeEdition
			with: FeEdition).
	table at: 422 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Archiver.U.markArchived.U.N2:with: with: 'VHHFn')
			with: FeArchiver
			with: FeEdition).
	table at: 423 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Archiver.U.restore.U.N3:with:with: with: 'HHHHFn')
			with: FeArchiver
			with: FeEdition
			with: FeEdition).
"Requests for class Array"
	table at: 468 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Array.U.copy.U.N1: with: 'HHFn')
			with: PrimArray).
	table at: 469 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.copy.U.N2:with: with: 'HHHFn')
			with: PrimArray
			with: PrimIntValue).
	table at: 255 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.copy.U.N3:with:with: with: 'HHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: PrimIntValue).
	table at: 424 storeValue: 
		(HHHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.copy.U.N4:with:with:with: with: 'HHHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: PrimIntValue
			with: PrimIntValue).
	table at: 425 storeValue: 
		(HHHHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.copy.U.N5:with:with:with:with: with: 'HHHHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: PrimIntValue
			with: PrimIntValue
			with: PrimIntValue).
	table at: 6 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Array.U.count.U.N1: with: 'HHFn')
			with: PrimArray).
	table at: 7 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #export0: with: 'VHFn')).
	table at: 256 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #export1: with: 'VHFn')).
	table at: 257 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #export2: with: 'VHFn')).
	table at: 8 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.get.U.N2:with: with: 'HHHFn')
			with: PrimArray
			with: PrimIntValue).
	table at: 9 storeValue: 
		(VHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.store.U.N3:with:with: with: 'VHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: Heaper).
	table at: 258 storeValue: 
		(VHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeAll.U.N1: with: 'VHFn')
			with: PrimArray).
	table at: 259 storeValue: 
		(VHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeAll.U.N2:with: with: 'VHHFn')
			with: PrimArray
			with: Heaper).
	table at: 260 storeValue: 
		(VHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeAll.U.N3:with:with: with: 'VHHHFn')
			with: PrimArray
			with: Heaper
			with: PrimIntValue).
	table at: 261 storeValue: 
		(VHHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeAll.U.N4:with:with:with: with: 'VHHHHFn')
			with: PrimArray
			with: Heaper
			with: PrimIntValue
			with: PrimIntValue).
	table at: 262 storeValue: 
		(VHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeMany.U.N3:with:with: with: 'VHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: PrimArray).
	table at: 263 storeValue: 
		(VHHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeMany.U.N4:with:with:with: with: 'VHHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: PrimArray
			with: PrimIntValue).
	table at: 264 storeValue: 
		(VHHHHHHandler make: (RequestHandler pointerToStaticMember: #Array.U.storeMany.U.N5:with:with:with:with: with: 'VHHHHHFn')
			with: PrimArray
			with: PrimIntValue
			with: PrimArray
			with: PrimIntValue
			with: PrimIntValue).
"Requests for class FloatArray"
	table at: 265 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #makeFloatArray: with: 'VHFn')).
	table at: 266 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #FloatArray.U.zeros.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 267 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #FloatArray.U.bitCount.U.N1: with: 'HHFn')
			with: PrimFloatArray).
"Requests for class HumberArray"
	table at: 268 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #makeHumberArray: with: 'VHFn')).
	table at: 269 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #HumberArray.U.zeros.U.N1: with: 'HHFn')
			with: PrimIntValue).
"Requests for class IntArray"
	table at: 10 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #makeIntArray: with: 'VHFn')).
	table at: 11 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #IntArray.U.zeros.U.N2:with: with: 'HHHFn')
			with: PrimIntValue
			with: PrimIntValue).
	table at: 12 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IntArray.U.bitCount.U.N1: with: 'HHFn')
			with: PrimIntArray).
"Requests for class PtrArray"
	table at: 270 storeValue: 
		(SpecialHandler make: (PromiseManager pointerToStaticMember: #makePtrArray: with: 'VHFn')).
	table at: 271 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #PtrArray.U.nulls.U.N1: with: 'HHFn')
			with: PrimIntValue).
"Requests for class Bundle"
	table at: 13 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #Bundle.U.region.U.N1: with: 'HHFn')
			with: FeBundle).
"Requests for class ArrayBundle"
	table at: 14 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ArrayBundle.U.array.U.N1: with: 'HHFn')
			with: FeArrayBundle).
	table at: 15 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ArrayBundle.U.ordering.U.N1: with: 'HHFn')
			with: FeArrayBundle).
"Requests for class ElementBundle"
	table at: 16 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #ElementBundle.U.element.U.N1: with: 'HHFn')
			with: FeElementBundle).
"Requests for class PlaceHolderBundle"
"Requests for class CoordinateSpace"
	table at: 272 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CoordinateSpace.U.ascending.U.N1: with: 'HHFn')
			with: CoordinateSpace).
	table at: 273 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CoordinateSpace.U.completeMapping.U.N2:with: with: 'HHHFn')
			with: CoordinateSpace
			with: XnRegion).
	table at: 274 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CoordinateSpace.U.descending.U.N1: with: 'HHFn')
			with: CoordinateSpace).
	table at: 17 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CoordinateSpace.U.emptyRegion.U.N1: with: 'HHFn')
			with: CoordinateSpace).
	table at: 18 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CoordinateSpace.U.fullRegion.U.N1: with: 'HHFn')
			with: CoordinateSpace).
	table at: 19 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CoordinateSpace.U.identityMapping.U.N1: with: 'HHFn')
			with: CoordinateSpace).
"Requests for class CrossSpace"
	table at: 20 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.make.U.N1: with: 'HHFn')
			with: PtrArray).
	table at: 275 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.axes.U.N1: with: 'HHFn')
			with: CrossSpace).
	table at: 276 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.axis.U.N2:with: with: 'HHHFn')
			with: CrossSpace
			with: PrimIntValue).
	table at: 277 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.axisCount.U.N1: with: 'HHFn')
			with: CrossSpace).
	table at: 278 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfMappings.U.N1: with: 'HHFn')
			with: CrossSpace).
	table at: 279 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfMappings.U.N2:with: with: 'HHHFn')
			with: CrossSpace
			with: PtrArray).
	table at: 426 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfOrderSpecs.U.N1: with: 'HHFn')
			with: CrossSpace).
	table at: 427 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfOrderSpecs.U.N2:with: with: 'HHHFn')
			with: CrossSpace
			with: PtrArray).
	table at: 428 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfOrderSpecs.U.N3:with:with: with: 'HHHHFn')
			with: CrossSpace
			with: PtrArray
			with: PrimIntArray).
	table at: 21 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfPositions.U.N2:with: with: 'HHHFn')
			with: CrossSpace
			with: PtrArray).
	table at: 22 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.crossOfRegions.U.N2:with: with: 'HHHFn')
			with: CrossSpace
			with: PtrArray).
	table at: 23 storeValue: 
		(HHHHHandler make: (RequestHandler pointerToStaticMember: #CrossSpace.U.extrusion.U.N3:with:with: with: 'HHHHFn')
			with: CrossSpace
			with: PrimIntValue
			with: XnRegion).
"Requests for class FilterSpace"
	table at: 280 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #FilterSpace.U.make.U.N1: with: 'HHFn')
			with: CoordinateSpace).
	table at: 24 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #FilterSpace.U.allFilter.U.N2:with: with: 'HHHFn')
			with: FilterSpace
			with: XnRegion).
	table at: 25 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #FilterSpace.U.anyFilter.U.N2:with: with: 'HHHFn')
			with: FilterSpace
			with: XnRegion).
	table at: 281 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #FilterSpace.U.baseSpace.U.N1: with: 'HHFn')
			with: FilterSpace).
	table at: 429 storeValue: 
		(HHHHandler make: (RequestHandler pointerToStaticMember: #FilterSpace.U.position.U.N2:with: with: 'HHHFn')
			with: FilterSpace
			with: XnRegion).
"Requests for class IDSpace"
	table at: 26 storeValue: 
		(HHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.global.U.N0 with: 'HFn')).
	table at: 282 storeValue: 
		(HHHandler make: (RequestHandler pointerToStaticMember: #IDSpace.U.import.U.N1: with: 'HHFn')
			with: PrimIntArray).
	self fillRequestTable1: table.!
*/
}
public PromiseManager() {
/*

Generated during transformation
*/
}
public PromiseManager(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
