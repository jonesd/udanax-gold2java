/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.primtab.PrimIndexTableStepper;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Map possibly wimpy pointers to integers.  Common usage almost never does a
 * remove on this class, therefore we rehash in order to save time on other ops.
 */
public class PrimIndexTable extends Heaper {

	protected PtrArray myPtrs;
	protected IntegerVarArray myIndices;
	protected int myTally;
	protected boolean amWimpy;
	protected int myOriginalSize;
/*
udanax-top.st:33282:
Heaper subclass: #PrimIndexTable
	instanceVariableNames: '
		myPtrs {PtrArray}
		myIndices {IntegerVarArray}
		myTally {Int4}
		amWimpy {BooleanVar}
		myOriginalSize {Int4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:33291:
PrimIndexTable comment:
'Map possibly wimpy pointers to integers.  Common usage almost never does a
remove on this class, therefore we rehash in order to save time on other ops.'!
*/
/*
udanax-top.st:33294:
(PrimIndexTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimIndexTable -/
friend SPTR(PrimIndexTable) primIndexTable (Int4 size);
friend SPTR(PrimIndexTable) wimpyIndexTable (Int4 size);
friend class PrimIndexTableTester;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:33440:
PrimIndexTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33443:
(PrimIndexTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimIndexTable -/
friend SPTR(PrimIndexTable) primIndexTable (Int4 size);
friend SPTR(PrimIndexTable) wimpyIndexTable (Int4 size);
friend class PrimIndexTableTester;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIndexTable.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void introduce(Heaper ptr, int index) {
	int loc;
	/* redundant code for sake of speed */
	loc = hashFind(ptr);
	if ((myPtrs.fetch(loc)) == null) {
		myIndices.storeIntegerVar(loc, index);
		myPtrs.store(loc, ptr);
	}
	else {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
	myTally = myTally + 1;
	if (myTally > (myPtrs.count() >> 1)) {
		grow();
	}
/*
udanax-top.st:33304:PrimIndexTable methodsFor: 'accessing'!
{void} at: ptr {Heaper} introduce: index {IntegerVar}
	| loc {Int32} |
	"redundant code for sake of speed"
	loc := self hashFind: ptr.
	(myPtrs fetch: loc) == NULL ifTrue: [
		myIndices at: loc storeIntegerVar: index.
		myPtrs at: loc store: ptr]
	ifFalse:
			[Heaper BLAST: #AlreadyInTable].
	myTally := myTally + 1.
	myTally > (myPtrs count bitShiftRight: 1) ifTrue: [ self grow ]!
*/
}
public void store(Heaper ptr, int index) {
	int loc;
	loc = hashFind(ptr);
	if ( ! ((myPtrs.fetch(loc)) != null)) {
		myTally = myTally + 1;
	}
	myIndices.storeIntegerVar(loc, index);
	myPtrs.store(loc, ptr);
	if (myTally > (myPtrs.count() >> 1)) {
		grow();
	}
/*
udanax-top.st:33316:PrimIndexTable methodsFor: 'accessing'!
{void} at: ptr {Heaper} store: index {IntegerVar}
	| loc {Int32} |
	loc := self hashFind: ptr.
	(myPtrs fetch: loc) ~~ NULL ifFalse:
		[ myTally := myTally + 1 ].
	myIndices at: loc storeIntegerVar: index.
	myPtrs at: loc store: ptr.
	myTally > (myPtrs count bitShiftRight: 1) ifTrue: [ self grow ]!
*/
}
/**
 * Clear all entries from the table. I know this looks like a hack, but the
 * alternative is to throw away the table and build a new one: an expensive
 * prospect for comm.
 */
public void clearAll() {
	if (myPtrs.count() > myOriginalSize) {
		myPtrs.destroy();
		if (amWimpy) {
			myPtrs = WeakPtrArray.make(XnExecutor.noopExecutor(), myOriginalSize);
		}
		else {
			myPtrs = PtrArray.nulls(myOriginalSize);
		}
	}
	else {
		myPtrs.storeAll();
	}
	myTally = 0;
/*
udanax-top.st:33325:PrimIndexTable methodsFor: 'accessing'!
{void} clearAll
	"Clear all entries from the table. I know this looks like a hack, but the 
	alternative is to throw away the table and build a new one: an expensive 
	prospect for comm."
	myPtrs count > myOriginalSize
		ifTrue:
			[myPtrs destroy.
			amWimpy 
				ifTrue: [myPtrs := WeakPtrArray make: XnExecutor noopExecutor with: myOriginalSize]
				ifFalse: [myPtrs := PtrArray nulls: myOriginalSize]]
		ifFalse:
			[myPtrs storeAll].
	myTally _ Int32Zero!
*/
}
public int count() {
	return myTally;
/*
udanax-top.st:33340:PrimIndexTable methodsFor: 'accessing'!
{Int32 INLINE} count
	^myTally!
*/
}
/**
 * return -1 on not found.
 */
public int fetch(Heaper ptr) {
	int loc;
	loc = hashFindFetch(ptr);
	if (loc == -1) {
		return -1;
	}
	return myIndices.integerVarAt(loc);
/*
udanax-top.st:33343:PrimIndexTable methodsFor: 'accessing'!
{IntegerVar} fetch: ptr {Heaper}
	"return -1 on not found."
	| loc {Int32} |
	loc _ self hashFindFetch: ptr.
	loc == -1 ifTrue: [ ^ -1 ].
	^ myIndices integerVarAt: loc!
*/
}
public int get(Heaper ptr) {
	int loc;
	loc = hashFind(ptr);
	if ((myPtrs.fetch(loc)) == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return myIndices.integerVarAt(loc);
/*
udanax-top.st:33350:PrimIndexTable methodsFor: 'accessing'!
{IntegerVar} get: ptr {Heaper}
	| loc {Int32} |
	loc _ self hashFind: ptr.
	(myPtrs fetch: loc) == NULL ifTrue:
		[ Heaper BLAST: #NotInTable ].
	^ myIndices integerVarAt: loc!
*/
}
public void remove(Heaper ptr) {
	int loc;
	loc = hashFind(ptr);
	if ((myPtrs.fetch(loc)) == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	myPtrs.store(loc, null);
	myTally = myTally - 1;
	rehash(myPtrs, myIndices, myPtrs.count());
/*
udanax-top.st:33357:PrimIndexTable methodsFor: 'accessing'!
{void} remove: ptr {Heaper}
	| loc {Int32} |
	loc := self hashFind: ptr.
	(myPtrs fetch: loc) == NULL ifTrue:
		[ Heaper BLAST: #NotInTable ].
	myPtrs at: loc store: NULL.
	myTally := myTally - 1.
	self rehash: myPtrs with: myIndices with: myPtrs count!
*/
}
public PrimIndexTable(int size, boolean wimpy) {
	super();
	amWimpy = wimpy;
	if (amWimpy) {
		myPtrs = WeakPtrArray.make(XnExecutor.noopExecutor(), size);
	}
	else {
		myPtrs = PtrArray.nulls(size);
	}
	myIndices = IntegerVarArray.zeros(size);
	myTally = 0;
	myOriginalSize = size;
/*
udanax-top.st:33368:PrimIndexTable methodsFor: 'protected:'!
create: size {Int32} with: wimpy {BooleanVar}
	super create.
	amWimpy := wimpy.
	amWimpy ifTrue: [myPtrs := WeakPtrArray make: XnExecutor noopExecutor with: size]
		ifFalse: [myPtrs := PtrArray nulls: size].
	myIndices := IntegerVarArray zeros: size.
	myTally := Int32Zero.
	myOriginalSize := size.!
*/
}
public void destruct() {
	myPtrs.destroy();
	myIndices.destroy();
	super.destruct();
/*
udanax-top.st:33377:PrimIndexTable methodsFor: 'protected:'!
{void} destruct
	myPtrs destroy.
	myIndices destroy.
	super destruct!
*/
}
public void grow() {
	rehash(myPtrs, myIndices, 5 * myPtrs.count() / 3);
/*
udanax-top.st:33384:PrimIndexTable methodsFor: 'private:'!
{void} grow
	self rehash: myPtrs with: myIndices with: 5 * myPtrs count // 3!
*/
}
public int hashFind(Heaper value) {
	int loc;
	int start;
	int top;
	Heaper tmp;
	loc = value.hashForEqual();
	top = myPtrs.count();
	loc = AboraSupport.modulo((FHash.fastHashUInt32(loc)), top);
	start = loc;
	while ((tmp = myPtrs.fetch(loc)) != null) {
		if (tmp == value) {
			return loc;
		}
		loc = loc + 1;
		if (loc == start) {
			throw new AboraRuntimeException(AboraRuntimeException.SANITY_VIOLATION);
		}
		if (loc >= top) {
			loc = 0;
		}
	}
	return loc;
/*
udanax-top.st:33387:PrimIndexTable methodsFor: 'private:'!
{Int32} hashFind: value {Heaper}
	| loc {Int32} start {Int32} top {Int32} tmp {Heaper wimpy} |
	loc := value hashForEqual.
	top := myPtrs count.
	loc :=  (FHash fastHash.UInt32: loc) \\ top.
	start := loc.
	[(tmp _ myPtrs fetch: loc) ~~ NULL] whileTrue:
		[tmp == value ifTrue: [ ^ loc ].
		loc := loc + 1.
		loc = start ifTrue: [ Heaper BLAST: #SanityViolation ].
		loc >= top ifTrue: 
			[loc := Int32Zero]].
	^ loc!
*/
}
public int hashFindFetch(Heaper value) {
	int hashLoc;
	int loc;
	int top;
	Heaper tmp;
	if (value == null) {
		return -1;
	}
	hashLoc = value.hashForEqual();
	top = myPtrs.count();
	hashLoc = AboraSupport.modulo((FHash.fastHashUInt32(hashLoc)), top);
	loc = hashLoc;
	while ((tmp = myPtrs.fetch(loc)) != null) {
		if (tmp == value) {
			return loc;
		}
		loc = loc + 1;
		if (loc == hashLoc) {
			return -1;
		}
		if (loc >= top) {
			loc = 0;
		}
	}
	return -1;
/*
udanax-top.st:33401:PrimIndexTable methodsFor: 'private:'!
{Int32} hashFindFetch: value {Heaper}
	| hashLoc {Int32} loc {Int32} top {Int32} tmp {Heaper wimpy} |
	value == NULL ifTrue:[ ^ -1].
	hashLoc := value hashForEqual.
	top := myPtrs count.
	hashLoc :=  (FHash fastHash.UInt32: hashLoc) \\ top.
	loc := hashLoc.
	[(tmp _ myPtrs fetch: loc) ~~ NULL] whileTrue:
		[tmp == value ifTrue: [ ^ loc ].
		loc := loc + 1.
		loc == hashLoc ifTrue: [^ -1].
		loc >= top ifTrue: 
			[loc := Int32Zero]].
	^ -1!
*/
}
public void rehash(PtrArray oldPtrs, IntegerVarArray oldIndices, int newSize) {
	if (amWimpy) {
		myPtrs = WeakPtrArray.make(XnExecutor.noopExecutor(), newSize);
	}
	else {
		myPtrs = PtrArray.nulls(newSize);
	}
	myIndices = IntegerVarArray.zeros(newSize);
	for (int i = 0; i < oldPtrs.count(); i ++ ) {
		int loc;
		if ((oldPtrs.fetch(i)) != null) {
			loc = hashFind((oldPtrs.fetch(i)));
			myIndices.storeIntegerVar(loc, (oldIndices.integerVarAt(i)));
			myPtrs.store(loc, (oldPtrs.fetch(i)));
		}
	}
	oldPtrs.destroy();
	oldIndices.destroy();
/*
udanax-top.st:33416:PrimIndexTable methodsFor: 'private:'!
{void} rehash: oldPtrs {PtrArray} with: oldIndices {IntegerVarArray} with: newSize {Int32}
	amWimpy ifTrue: [myPtrs := WeakPtrArray make: XnExecutor noopExecutor with: newSize]
		ifFalse: [myPtrs := PtrArray nulls: newSize].
	myIndices := IntegerVarArray zeros: newSize.
	Int32Zero almostTo: oldPtrs count do: [:i {Int32} |
		| loc {Int32} |
		(oldPtrs fetch: i) ~~ NULL ifTrue:
			[loc := self hashFind: (oldPtrs fetch: i).
			myIndices at: loc storeIntegerVar: (oldIndices integerVarAt: i).
			myPtrs at: loc store: (oldPtrs fetch: i)]].
	oldPtrs destroy.
	oldIndices destroy!
*/
}
public int actualHashForEqual() {
	return myTally ^ myPtrs.count();
/*
udanax-top.st:33431:PrimIndexTable methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^  myTally bitXor: myPtrs count.!
*/
}
public PrimIndexTableStepper stepper() {
	return new PrimIndexTableStepper(myPtrs, myIndices, 0);
/*
udanax-top.st:33436:PrimIndexTable methodsFor: 'enumerating'!
{PrimIndexTableStepper} stepper
	^ PrimIndexTableStepper create: myPtrs with: myIndices with: Int32Zero!
*/
}
public static PrimIndexTable make(int size) {
	return new PrimIndexTable(size, false);
/*
udanax-top.st:33453:PrimIndexTable class methodsFor: 'create'!
make: size {Int32}
	^ self create: size with: false!
*/
}
public static PrimIndexTable wimpyIndexTable(int size) {
	return new PrimIndexTable(size, true);
/*
udanax-top.st:33456:PrimIndexTable class methodsFor: 'create'!
{PrimIndexTable} wimpyIndexTable: size {Int32}
	^ self create: size with: true!
*/
}
public PrimIndexTable() {
/*

Generated during transformation
*/
}
public PrimIndexTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
