/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.cxx.otherclass.CopyRecipe;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.CommIbid;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.DeletedHeaper;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * myIbids maps from ibid number to already sent objects.  The ibids table is explicitly
 * managed as a PtrArray because it is such a bottleneck.
 */
public class SpecialistRcvr extends Rcvr {

	protected TransferSpecialist mySpecialist;
	protected PtrArray myIbids;
	protected int myNextIbid;
	protected static PtrArray RcvrIbidCache;
/*
udanax-top.st:41070:
Rcvr subclass: #SpecialistRcvr
	instanceVariableNames: '
		mySpecialist {TransferSpecialist}
		myIbids {PtrArray}
		myNextIbid {Int4}'
	classVariableNames: 'RcvrIbidCache {PtrArray} '
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:41077:
SpecialistRcvr comment:
'myIbids maps from ibid number to already sent objects.  The ibids table is explicitly managed as a PtrArray because it is such a bottleneck.'!
*/
/*
udanax-top.st:41079:
(SpecialistRcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:41229:
SpecialistRcvr class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:41232:
(SpecialistRcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SpecialistRcvr.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public boolean receiveBooleanVar() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41084:SpecialistRcvr methodsFor: 'receiving'!
{BooleanVar} receiveBooleanVar
	self subclassResponsibility!
*/
}
/**
 * Return a category object using the internal coding that any
 * rcvr must have to represent categories.
 */
public Category receiveCategory() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41087:SpecialistRcvr methodsFor: 'receiving'!
{Category} receiveCategory
	"Return a category object using the internal coding that any 
	  rcvr must have to represent categories."
	  
	 self subclassResponsibility!
*/
}
/**
 * Fill the array with data from the stream.
 */
public void receiveData(UInt8Array array) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41093:SpecialistRcvr methodsFor: 'receiving'!
{void} receiveData: array {UInt8Array}
	"Fill the array with data from the stream."
	
	self subclassResponsibility!
*/
}
/**
 * receive the next heaper
 */
public Heaper receiveHeaper() {
	Category cat;
	cat = fetchStartOfInstance();
	if (cat == null) {
		return null;
	}
	if (cat.isEqual(AboraSupport.findCategory(CommIbid.class))) {
		int ibidNum;
		Heaper result;
		ibidNum = receiveInt32();
		endOfInstance();
		result = myIbids.fetch(ibidNum);
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
		}
		return result;
	}
	else {
		Heaper result1;
		result1 = mySpecialist.receiveHeaperFrom(cat, this);
		endOfInstance();
		return result1;
	}
/*
udanax-top.st:41098:SpecialistRcvr methodsFor: 'receiving'!
{Heaper} receiveHeaper
	"receive the next heaper"
	| cat {Category} |
	cat _ self fetchStartOfInstance.
	cat == NULL ifTrue: [^NULL]. 
	(cat isEqual: CommIbid) ifTrue:
		[| ibidNum {Int32} result {Heaper} |
		ibidNum _ self receiveInt32. 
		self endOfInstance.
		result _ myIbids fetch: ibidNum.
		result == NULL ifTrue: [Heaper BLAST: #NotInTable].
		^result]
	ifFalse: [| result {Heaper} |
			result _ mySpecialist receiveHeaper: cat from: self.
			self endOfInstance.
			^result]!
*/
}
public double receiveIEEEDoubleVar() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41116:SpecialistRcvr methodsFor: 'receiving'!
{IEEEDoubleVar} receiveIEEEDoubleVar
	self subclassResponsibility!
*/
}
public int receiveInt32() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41119:SpecialistRcvr methodsFor: 'receiving'!
{Int32} receiveInt32
	self subclassResponsibility!
*/
}
public int receiveInt8() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41122:SpecialistRcvr methodsFor: 'receiving'!
{Int8} receiveInt8
	self subclassResponsibility!
*/
}
public int receiveIntegerVar() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41125:SpecialistRcvr methodsFor: 'receiving'!
{IntegerVar} receiveIntegerVar
	self subclassResponsibility!
*/
}
/**
 * Receive an object into another object.
 */
public void receiveInto(Heaper memory) {
	Category cat;
	cat = fetchStartOfInstance();
	if (cat == null || (cat.isEqual(AboraSupport.findCategory(CommIbid.class)))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_BECOMABLE);
	}
	mySpecialist.receiveHeaperIntoFrom(cat, memory, this);
	endOfInstance();
/*
udanax-top.st:41128:SpecialistRcvr methodsFor: 'receiving'!
{void} receiveInto: memory {Heaper}
	"Receive an object into another object."
	| cat {Category} |
	cat _ self fetchStartOfInstance. 
	(cat == NULL or: [cat isEqual: CommIbid]) ifTrue: [Heaper BLAST: #NotBecomable].
	mySpecialist receiveHeaper: cat into: memory from: self.
	self endOfInstance!
*/
}
public String receiveString() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41138:SpecialistRcvr methodsFor: 'receiving'!
{char star} receiveString
	self subclassResponsibility!
*/
}
public int receiveUInt32() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41142:SpecialistRcvr methodsFor: 'receiving'!
{UInt32} receiveUInt32
	self subclassResponsibility!
*/
}
public int receiveUInt8() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41145:SpecialistRcvr methodsFor: 'receiving'!
{UInt8} receiveUInt8
	self subclassResponsibility!
*/
}
/**
 * Pull the contents of the next heaper off the wire.
 */
public Heaper basicReceive(Recipe recipe) {
	if (recipe instanceof CopyRecipe) {
		CopyRecipe copy = (CopyRecipe) recipe;
		return copy.parse(this);
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.BAD_RECIPE);
	}
/*
udanax-top.st:41150:SpecialistRcvr methodsFor: 'specialist receiving'!
{Heaper} basicReceive: recipe {Recipe}
	"Pull the contents of the next heaper off the wire."
	recipe cast: CopyRecipe into: [:copy | ^copy parse: self]
		others: [Heaper BLAST: #BadRecipe].
	^NULL!
*/
}
/**
 * Pull the contents of the next heaper off the wire.
 */
public void basicReceiveInto(Recipe recipe, Heaper memory) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:41157:SpecialistRcvr methodsFor: 'specialist receiving'!
{void} basicReceive: recipe {Recipe} into: memory {Heaper} 
	"Pull the contents of the next heaper off the wire."
	(memory getCategory canYouBecome: recipe categoryOfDish)
		ifFalse: [Heaper BLAST: #NotBecomable].
	self registerIbid: memory.
	[memory destructor] smalltalkOnly.
	recipe cast: CopyRecipe into: [:copy | copy parse: self into: memory]
		others: [Heaper BLAST: #BadRecipe]!
*/
}
/**
 * Create and register a memory slot for an instance of the given category.
 */
public Heaper makeIbid(Category cat) {
	Heaper result;
	AboraSupport.translateOnly();
	{
		/* result = (Heaper *)Heaper::operator new (0, xcsj, cat); */
	}
	AboraSupport.smalltalkOnly();
	{
		result = new DeletedHeaper();
	}
	registerIbid(result);
	return result;
/*
udanax-top.st:41167:SpecialistRcvr methodsFor: 'specialist receiving'!
{Heaper} makeIbid: cat {Category}
	"Create and register a memory slot for an instance of the given category."
	| result {Heaper} |
	'result = (Heaper *)Heaper::operator new (0, xcsj, cat);' translateOnly.
	[result _ DeletedHeaper create] smalltalkOnly.
	self registerIbid: result.
	^result!
*/
}
public void registerIbid(Heaper obj) {
	if (myNextIbid >= myIbids.count()) {
		/* Grow the table. */
		PtrArray oldIbids;
		oldIbids = myIbids;
		myIbids = PtrArray.nulls(myNextIbid * 2);
		for (int i = 1; i < myNextIbid; i ++ ) {
			myIbids.store(i, (oldIbids.fetch(i)));
		}
		oldIbids.destroy();
	}
	myIbids.store(myNextIbid, obj);
	myNextIbid = myNextIbid + 1;
/*
udanax-top.st:41175:SpecialistRcvr methodsFor: 'specialist receiving'!
{void} registerIbid: obj {Heaper}
 
 	myNextIbid >= myIbids count ifTrue:
 		["Grow the table."
 		| oldIbids {PtrArray} |
 		oldIbids _ myIbids.
 		myIbids _ PtrArray nulls: myNextIbid * 2.
 		1 almostTo: myNextIbid do: [:i {Int32} | myIbids at: i store: (oldIbids fetch: i)].
 		oldIbids destroy].
	myIbids at: myNextIbid store: obj.
	myNextIbid _ myNextIbid + 1.!
*/
}
public void endOfInstance() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41189:SpecialistRcvr methodsFor: 'protected: specialist'!
{void} endOfInstance
	
	self subclassResponsibility!
*/
}
public void endPacket() {
	for (int i = 0; i < myNextIbid; i ++ ) {
		myIbids.store(i, null);
	}
	myNextIbid = 0;
/*
udanax-top.st:41193:SpecialistRcvr methodsFor: 'protected: specialist'!
{void} endPacket
	Int32Zero almostTo: myNextIbid do: 
		[:i {Int32} |
		myIbids at: i store: NULL].
	myNextIbid _ Int32Zero!
*/
}
public Category fetchStartOfInstance() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41199:SpecialistRcvr methodsFor: 'protected: specialist'!
{Category} fetchStartOfInstance
	self subclassResponsibility!
*/
}
public TransferSpecialist specialist() {
	return mySpecialist;
/*
udanax-top.st:41203:SpecialistRcvr methodsFor: 'protected: specialist'!
{TransferSpecialist} specialist
	^mySpecialist!
*/
}
public SpecialistRcvr(TransferSpecialist specialist) {
	super();
	mySpecialist = specialist;
	if (RcvrIbidCache == null) {
		myIbids = PtrArray.nulls(128);
	}
	else {
		myIbids = RcvrIbidCache;
		RcvrIbidCache = null;
	}
	myNextIbid = 0;
/*
udanax-top.st:41208:SpecialistRcvr methodsFor: 'protected: creation'!
create: specialist {TransferSpecialist}
	super create.
	mySpecialist _ specialist.
	RcvrIbidCache == NULL
		ifTrue: [myIbids _ PtrArray nulls: 128]
		ifFalse:
			[myIbids _ RcvrIbidCache.
			RcvrIbidCache _ NULL].
	myNextIbid _ Int32Zero!
*/
}
public void destruct() {
	if (RcvrIbidCache == null) {
		myIbids.storeAll();
		RcvrIbidCache = myIbids;
		myIbids = null;
	}
	else {
		myIbids.destroy();
	}
	/* mySpecialist destroy */
	super.destruct();
/*
udanax-top.st:41218:SpecialistRcvr methodsFor: 'protected: creation'!
{void} destruct
	RcvrIbidCache == NULL 
		ifTrue:
			[myIbids storeAll.
			RcvrIbidCache _ myIbids cast: PtrArray.
			myIbids _ NULL]
		ifFalse: [myIbids destroy].
	"mySpecialist destroy"
	super destruct!
*/
}
public static void linkTimeNonInherited() {
	RcvrIbidCache = null;
/*
udanax-top.st:41237:SpecialistRcvr class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	RcvrIbidCache _ NULL!
*/
}
public SpecialistRcvr() {
/*

Generated during transformation
*/
}
public SpecialistRcvr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
