/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.urdi;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.urdi.SnarfHandle;
import info.dgjones.abora.gold.java.urdi.Urdi;
import info.dgjones.abora.gold.java.urdi.UrdiView;
import info.dgjones.abora.gold.urdi.SnarfInfoHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * The SnarfInfoHandler is an interface to the first few snarfs in an urdi that tells how
 * much space is unallocated in each of the remaining snarfs, and keeps a bit as to whether
 * any forgotten objects are in each snarf.
 * The data is kept packed in the first few snarfs with 4 bytes per snarf recorded.  The
 * forgotten bit is the high bit of each entry.
 * mySnarfs is a table of SnarfHandles onto the snarfInfo snarfs (the first few snarfs in the
 * Urdi).  You release those snarfs by destroying the snarfInfoHandler and creating a new one
 * when you want the information again.
 * myTotal is the total number of snarfs in the Urdi.
 */
public class SnarfInfoHandler extends Heaper {

	protected MuTable mySnarfs;
	protected int myTotal;
	protected SnarfHandle myCurrentHandle;
	protected int myCurrentStart;
	protected int myCurrentIndex;
	protected static int ForgottenFlag;
	protected static int SizeMask;
/*
udanax-top.st:52107:
Heaper subclass: #SnarfInfoHandler
	instanceVariableNames: '
		mySnarfs {MuTable of: SnarfHandle}
		myTotal {Int4}
		myCurrentHandle {SnarfHandle}
		myCurrentStart {Int4}
		myCurrentIndex {IntegerVar}'
	classVariableNames: '
		ForgottenFlag {Int4 const} 
		SizeMask {Int4 const} '
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:52118:
SnarfInfoHandler comment:
'The SnarfInfoHandler is an interface to the first few snarfs in an urdi that tells how much space is unallocated in each of the remaining snarfs, and keeps a bit as to whether any forgotten objects are in each snarf.
The data is kept packed in the first few snarfs with 4 bytes per snarf recorded.  The forgotten bit is the high bit of each entry.
mySnarfs is a table of SnarfHandles onto the snarfInfo snarfs (the first few snarfs in the Urdi).  You release those snarfs by destroying the snarfInfoHandler and creating a new one when you want the information again.
myTotal is the total number of snarfs in the Urdi.'!
*/
/*
udanax-top.st:52126:
(SnarfInfoHandler getOrMakeCxxClassDescription)
	friends:
'/- friends for class SnarfInfoHandler -/
friend class SnarfInfoStepper;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:52279:
SnarfInfoHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:52282:
(SnarfInfoHandler getOrMakeCxxClassDescription)
	friends:
'/- friends for class SnarfInfoHandler -/
friend class SnarfInfoStepper;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SnarfInfoHandler.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return the forgotten bit for the snarf at snarfID.
 */
public boolean getForgottenFlag(int snarfID) {
	int offset;
	offset = locate(snarfID);
	return ((myCurrentHandle.get32(offset)) & ForgottenFlag) != 0;
/*
udanax-top.st:52134:SnarfInfoHandler methodsFor: 'accessing'!
{BooleanVar} getForgottenFlag: snarfID {SnarfID} 
	"Return the forgotten bit for the snarf at snarfID."
	
	| offset {Int32} |
	offset _ self locate: snarfID.
	^((myCurrentHandle get32: offset) bitAnd: ForgottenFlag) ~~ Int32Zero!
*/
}
/**
 * Return the spaceLeft for the snarf at snarfID.
 */
public int getSpaceLeft(int snarfID) {
	int offset;
	offset = locate(snarfID);
	return (myCurrentHandle.get32(offset)) & SizeMask;
/*
udanax-top.st:52141:SnarfInfoHandler methodsFor: 'accessing'!
{Int32} getSpaceLeft: snarfID {SnarfID}
	"Return the spaceLeft for the snarf at snarfID."
	
	| offset {Int32} |
	offset _ self locate: snarfID.
	^(myCurrentHandle get32: offset) bitAnd: SizeMask!
*/
}
/**
 * Set or clear the forgotten bit for the snarf at snarfID.
 */
public void setForgottenFlag(int snarfID, boolean flag) {
	int offset;
	offset = locate(snarfID);
	myCurrentHandle.makeWritable();
	if (flag) {
		myCurrentHandle.put32(offset, ((myCurrentHandle.get32(offset)) | ForgottenFlag));
	}
	else {
		myCurrentHandle.put32(offset, ((myCurrentHandle.get32(offset)) & ~ ForgottenFlag));
	}
/*
udanax-top.st:52148:SnarfInfoHandler methodsFor: 'accessing'!
{void} setForgottenFlag: snarfID {SnarfID} with: flag {BooleanVar}
	"Set or clear the forgotten bit for the snarf at snarfID."
	
	| offset {Int32} |
	offset _ self locate: snarfID.
	myCurrentHandle makeWritable.
	flag ifTrue: [myCurrentHandle at: offset put32: ((myCurrentHandle get32: offset) bitOr: ForgottenFlag)]
		ifFalse: [myCurrentHandle at: offset put32: ((myCurrentHandle get32: offset) bitAnd: ForgottenFlag bitInvert)]!
*/
}
/**
 * Set the space for the snarf at snarfID.
 */
public void setSpaceLeft(int snarfID, int space) {
	int offset;
	offset = locate(snarfID);
	myCurrentHandle.makeWritable();
	myCurrentHandle.put32(offset, space + ((myCurrentHandle.get32(offset)) & ~ SizeMask));
/*
udanax-top.st:52157:SnarfInfoHandler methodsFor: 'accessing'!
{void} setSpaceLeft: snarfID {SnarfID} with: space {Int32}
	"Set the space for the snarf at snarfID."
	
	| offset {Int32} |
	offset _ self locate: snarfID.
	myCurrentHandle makeWritable.
	myCurrentHandle at: offset put32: space + ((myCurrentHandle get32: offset) bitAnd: SizeMask bitInvert)!
*/
}
/**
 * Return the total number of snarfs in the urdi.
 */
public int snarfCount() {
	return myTotal;
/*
udanax-top.st:52165:SnarfInfoHandler methodsFor: 'accessing'!
{Int32} snarfCount
	"Return the total number of snarfs in the urdi."
	
	^myTotal!
*/
}
/**
 * Return the number of snarfs that the snarf info information takes
 * up. This is used to know what snarf to get the first object from.
 */
public int snarfInfoCount() {
	return mySnarfs.count();
/*
udanax-top.st:52170:SnarfInfoHandler methodsFor: 'accessing'!
{Int32} snarfInfoCount
	"Return the number of snarfs that the snarf info information takes 
	up. This is used to know what snarf to get the first object from."
	^mySnarfs count DOTasLong!
*/
}
/**
 * Se the spaceLeft to a certain amount, and clear all the flags. This is used
 * when initializing the snarfInfo so we don't get confused by the flags.
 */
public void initializeSpaceLeft(int snarfID, int space) {
	int offset;
	offset = locate(snarfID);
	myCurrentHandle.makeWritable();
	myCurrentHandle.put32(offset, space);
/*
udanax-top.st:52178:SnarfInfoHandler methodsFor: 'private:'!
{void} initializeSpaceLeft: snarfID {SnarfID} with: space {Int32} 
	"Se the spaceLeft to a certain amount, and clear all the flags. This is used 
	when initializing the snarfInfo so we don't get confused by the flags."
	| offset {Int32} |
	offset _ self locate: snarfID.
	myCurrentHandle makeWritable.
	myCurrentHandle at: offset put32: space!
*/
}
/**
 * Return the snarfHandle for the snarfInfo snarf that contains the spaceLeft and forgotten
 * flag for the snarf at snarfID.
 */
public int locate(int snarfID) {
	if (myCurrentHandle != null && (snarfID >= myCurrentStart && (snarfID < (myCurrentStart + (myCurrentHandle.getDataSize() / 4))))) {
		return (snarfID - myCurrentStart) * 4;
	}
	if (snarfID < myCurrentStart || (myCurrentHandle == null)) {
		myCurrentIndex = 0;
		myCurrentHandle = (SnarfHandle) (mySnarfs.intGet(myCurrentIndex));
		myCurrentStart = 0;
	}
	while (myCurrentHandle != null) {
		int count;
		count = myCurrentHandle.getDataSize() / 4;
		if (snarfID < (count + myCurrentStart)) {
			return (snarfID - myCurrentStart) * 4;
		}
		myCurrentIndex = myCurrentIndex + 1;
		myCurrentHandle = (SnarfHandle) (mySnarfs.intFetch(myCurrentIndex));
		myCurrentStart = myCurrentStart + count;
	}
	throw new AboraRuntimeException(AboraRuntimeException.NO_SNARF_INFO);
/*
udanax-top.st:52187:SnarfInfoHandler methodsFor: 'private:'!
{Int32} locate: snarfID {SnarfID}
	"Return the snarfHandle for the snarfInfo snarf that contains the spaceLeft and forgotten flag for the snarf at snarfID."
	(myCurrentHandle ~~ NULL
		and: [snarfID >= myCurrentStart 
		and: [snarfID < (myCurrentStart + (myCurrentHandle getDataSize // 4))]]) 
		ifTrue: [^(snarfID - myCurrentStart) * 4].
	(snarfID < myCurrentStart or: [myCurrentHandle == NULL]) ifTrue:
		[myCurrentIndex _ IntegerVar0.
		myCurrentHandle _ (mySnarfs intGet: myCurrentIndex) cast: SnarfHandle.
		myCurrentStart _ Int32Zero].
	[myCurrentHandle ~~ NULL] whileTrue:
		[| count {Int32} |
		count _ myCurrentHandle getDataSize // 4.
		snarfID < (count + myCurrentStart) ifTrue: [^(snarfID - myCurrentStart) * 4].
		myCurrentIndex _ myCurrentIndex + 1.
		myCurrentHandle _ (mySnarfs intFetch: myCurrentIndex) cast: SnarfHandle.
		myCurrentStart _ myCurrentStart + count].
	Heaper BLAST: #NoSnarfInfo.
	^Int32Zero!
*/
}
/**
 * Release all my handles before going away.
 */
public void destruct() {
	myCurrentHandle = null;
	if (mySnarfs.getCategory() != AboraSupport.findCategory(Heaper.class)) {
		Stepper stomper = mySnarfs.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			SnarfHandle handle = (SnarfHandle) stomper.fetch();
			if (handle == null) {
				continue ;
			}
			handle.destroy();
		}
		stomper.destroy();
	}
	mySnarfs = null;
	super.destruct();
/*
udanax-top.st:52210:SnarfInfoHandler methodsFor: 'protected: destruct'!
{void} destruct
	"Release all my handles before going away."
	myCurrentHandle _ NULL.
	mySnarfs getCategory ~= Heaper ifTrue:
		[mySnarfs stepper forEach:
			 [:handle {SnarfHandle} |
			 handle destroy]].
	mySnarfs _ NULL.
	super destruct!
*/
}
/**
 * This constructor is for a newly created urdi with no existing snarfInfo
 * information. Set the spaceLeft for each snarf to its maximum and clear the
 * forgotten flag. Note that this figures out how many snarfInfo snarfs to use on
 * the fly by allocating as many snarfInfo cells as it can in the first snarf, then
 * going on to the second snarf, until enough snarfInfo snarfs are allocated. Then
 * it goes through all the entries in the snarfInfo for each non-snarfInfo snarf
 * and set the spaceLeft appropriately.
 */
public SnarfInfoHandler(Urdi urdi, UrdiView view) {
	super();
	int snarfID;
	int total;
	snarfID = 0;
	myTotal = urdi.usableSnarfs();
	mySnarfs = MuArray.array();
	myCurrentStart = 0;
	myCurrentIndex = 0;
	myCurrentHandle = null;
	total = 0;
	/* Initialize enough snarfInfo snarfs for all snarfs in the Urdi. */
	while (total < myTotal) {
		SnarfHandle handle;
		handle = view.makeErasingHandle(snarfID);
		mySnarfs.intIntroduce(snarfID, handle);
		initializeSpaceLeft(snarfID, 0);
		total = total + (handle.getDataSize() / 4);
		snarfID = snarfID + 1;
	}
	for (int dataSnarfID = 
	/* Initialize the entries for all non-snarfInfo snarfs. */
	snarfID; dataSnarfID < myTotal; dataSnarfID ++ ) {
		initializeSpaceLeft(dataSnarfID, (urdi.getDataSizeOfSnarf(dataSnarfID)));
	}
/*
udanax-top.st:52222:SnarfInfoHandler methodsFor: 'create'!
create.Urdi: urdi {Urdi} with: view {UrdiView}
	"This constructor is for a newly created urdi with no existing snarfInfo 
	information. Set the spaceLeft for each snarf to its maximum and clear the 
	forgotten flag. Note that this figures out how many snarfInfo snarfs to use on 
	the fly by allocating as many snarfInfo cells as it can in the first snarf, then 
	going on to the second snarf, until enough snarfInfo snarfs are allocated. Then 
	it goes through all the entries in the snarfInfo for each non-snarfInfo snarf 
	and set the spaceLeft appropriately."
	
 	| snarfID {SnarfID} total {Int32} |
	super create.
	snarfID _ Int32Zero.
	myTotal _ urdi usableSnarfs.
	mySnarfs _ MuArray array.
	myCurrentStart _ Int32Zero.
	myCurrentIndex _ IntegerVar0.
	myCurrentHandle _ NULL.
	total _ Int32Zero.
	"Initialize enough snarfInfo snarfs for all snarfs in the Urdi."
	[total < myTotal] whileTrue:
		[| handle {SnarfHandle} |
		handle _ view makeErasingHandle: snarfID.
		mySnarfs atInt: snarfID introduce: handle.
		self initializeSpaceLeft: snarfID with: Int32Zero.
		total _ total + (handle getDataSize // 4).
		snarfID _ snarfID + 1].
	"Initialize the entries for all non-snarfInfo snarfs."
	snarfID almostTo: myTotal do: 
		[:dataSnarfID {SnarfID} |
		self initializeSpaceLeft: dataSnarfID with: (urdi getDataSizeOfSnarf: dataSnarfID)]!
*/
}
/**
 * This constructor is for reopening an existing urdi and using its existing snarfInfo.
 * Read snarfs until it has enough cells for all snarfs in fthe urdi.
 */
public SnarfInfoHandler(UrdiView view, Urdi urdi) {
	super();
	int snarfID;
	int total;
	snarfID = 0;
	myTotal = urdi.usableSnarfs();
	mySnarfs = MuArray.array();
	myCurrentStart = 0;
	myCurrentIndex = 0;
	myCurrentHandle = null;
	total = 0;
	while (total < myTotal) {
		SnarfHandle handle;
		handle = view.makeReadHandle(snarfID);
		mySnarfs.intIntroduce(snarfID, handle);
		total = total + (handle.getDataSize() / 4);
		snarfID = snarfID + 1;
	}
/*
udanax-top.st:52253:SnarfInfoHandler methodsFor: 'create'!
create.UrdiView: view {UrdiView} with: urdi {Urdi}
	"This constructor is for reopening an existing urdi and using its existing snarfInfo.
	 Read snarfs until it has enough cells for all snarfs in fthe urdi."
	| snarfID {SnarfID} total {Int32} |
	super create.
	snarfID _ Int32Zero.
	myTotal _ urdi usableSnarfs.
	mySnarfs _ MuArray array.
	myCurrentStart _ Int32Zero.
	myCurrentIndex _ IntegerVar0.
	myCurrentHandle _ NULL.
	total _ UInt32Zero.
	[total < myTotal] whileTrue:
		[| handle {SnarfHandle} |
		handle _ view makeReadHandle: snarfID.
		mySnarfs atInt: snarfID introduce: handle.
		total _ total + (handle getDataSize // 4).
		snarfID _ snarfID + 1]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:52275:SnarfInfoHandler methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static void initializeSnarfInfo(Urdi urdi, UrdiView view) {
	SnarfInfoHandler handler;
	handler = new SnarfInfoHandler(urdi, view);
	handler.destroy();
/*
udanax-top.st:52290:SnarfInfoHandler class methodsFor: 'pcreate'!
{void} initializeSnarfInfo: urdi {Urdi} with: view {UrdiView}
	| handler {SnarfInfoHandler} |
	handler _ self create.Urdi: urdi with: view.
	handler destroy!
*/
}
public static SnarfInfoHandler make(Urdi urdi, UrdiView view) {
	return new SnarfInfoHandler(view, urdi);
/*
udanax-top.st:52295:SnarfInfoHandler class methodsFor: 'pcreate'!
make: urdi {Urdi} with: view {UrdiView}
	^self create.UrdiView: view with: urdi!
*/
}
/**
 * self hack.
 */
public static void linkTimeNonInherited() {
	/* These don't use the full 32 bits so that we don't start manipulating LargeIntegers. */
	ForgottenFlag = 1 << 24;
	SizeMask = 
	/* ForgottenFlag - 1 */
	(1 << 24) - 1;
/*
udanax-top.st:52300:SnarfInfoHandler class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	"self hack."	"These don't use the full 32 bits so that we don't start manipulating LargeIntegers."
	ForgottenFlag _ 1 bitShift: 24.
	SizeMask _ "ForgottenFlag - 1"(1 bitShift: 24) - 1.!
*/
}
public static SnarfInfoHandler create(Urdi urdi, UrdiView view) {
	return new SnarfInfoHandler(urdi, view);
/*
udanax-top.st:52308:SnarfInfoHandler class methodsFor: 'smalltalk: create'!
create.Urdi: urdi {Urdi} with: view {UrdiView}
	^self new create.Urdi: urdi with: view!
*/
}
public static SnarfInfoHandler create(UrdiView view, Urdi urdi) {
	return new SnarfInfoHandler(view, urdi);
/*
udanax-top.st:52311:SnarfInfoHandler class methodsFor: 'smalltalk: create'!
create.UrdiView: view {UrdiView} with: urdi {Urdi}
	^self new create.UrdiView: view with: urdi!
*/
}
public SnarfInfoHandler() {
/*

Generated during transformation
*/
}
public SnarfInfoHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
