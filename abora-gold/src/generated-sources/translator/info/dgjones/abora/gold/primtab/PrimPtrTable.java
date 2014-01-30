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
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.primtab.PrimPtrTableExecutor;
import info.dgjones.abora.gold.primtab.PrimPtrTableStepper;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Map integers to strong or weak pointers
 */
public class PrimPtrTable extends Heaper {

	protected PtrArray myPtrs;
	protected IntegerVarArray myIndices;
	protected int myTally;
	protected XnExecutor myExecutor;
/*
udanax-top.st:33595:
Heaper subclass: #PrimPtrTable
	instanceVariableNames: '
		myPtrs {PtrArray}
		myIndices {IntegerVarArray}
		myTally {Int4}
		myExecutor {XnExecutor | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:33603:
PrimPtrTable comment:
'Map integers to strong or weak pointers'!
*/
/*
udanax-top.st:33605:
(PrimPtrTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimPtrTable -/
friend class PrimPtrTableExecutor;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:33762:
PrimPtrTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33765:
(PrimPtrTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimPtrTable -/
friend class PrimPtrTableExecutor;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPtrTable.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void introduce(int index, Heaper ptr) {
	int loc;
	Heaper tmp;
	loc = hashFind(index);
	if ((tmp = myPtrs.fetch(loc)) != null && (tmp != PrimRemovedObject.make())) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
	myIndices.storeIntegerVar(loc, index);
	myPtrs.store(loc, ptr);
	myTally = myTally + 1;
	if (myTally > (2 * myPtrs.count() / 3)) {
		grow();
	}
/*
udanax-top.st:33613:PrimPtrTable methodsFor: 'accessing'!
{void} at: index {IntegerVar} introduce: ptr {Heaper}
	| loc {Int32} tmp {Heaper wimpy} |
	loc := self hashFind: index.
	((tmp _ myPtrs fetch: loc) ~~ NULL and: [tmp ~~ PrimRemovedObject make]) ifTrue:
		[ Heaper BLAST: #AlreadyInTable ].
	myIndices at: loc storeIntegerVar: index.
	myPtrs at: loc store: ptr.
	myTally := myTally + 1.
	myTally > (2 * myPtrs count / 3) ifTrue: [ self grow ]!
*/
}
public void store(int index, Heaper ptr) {
	int loc;
	Heaper tmp;
	loc = hashFind(index);
	if ( ! ((tmp = myPtrs.fetch(loc)) != null && (tmp != PrimRemovedObject.make()))) {
		myTally = myTally + 1;
	}
	myIndices.storeIntegerVar(loc, index);
	myPtrs.store(loc, ptr);
	if (myTally > (2 * myPtrs.count() / 3)) {
		grow();
	}
/*
udanax-top.st:33623:PrimPtrTable methodsFor: 'accessing'!
{void} at: index {IntegerVar} store: ptr {Heaper}
	| loc {Int32} tmp {Heaper wimpy} |
	loc := self hashFind: index.
	((tmp _ myPtrs fetch: loc) ~~ NULL and: [tmp ~~ PrimRemovedObject make]) ifFalse:
		[myTally := myTally + 1].
	myIndices at: loc storeIntegerVar: index.
	myPtrs at: loc store: ptr.
	myTally > (2 * myPtrs count / 3) ifTrue: [ self grow ]!
*/
}
/**
 * Clear all entries from the table. I know this looks like a hack, but the
 * alternative is to throw away the table and build a new one: an expensive
 * prospect for comm.
 */
public void clearAll() {
	myPtrs.storeAll();
	myTally = 0;
/*
udanax-top.st:33632:PrimPtrTable methodsFor: 'accessing'!
{void} clearAll
	"Clear all entries from the table. I know this looks like a hack, but the 
	alternative is to throw away the table and build a new one: an expensive 
	prospect for comm."
	myPtrs storeAll.
	myTally _ Int32Zero!
*/
}
public int count() {
	return myTally;
/*
udanax-top.st:33640:PrimPtrTable methodsFor: 'accessing'!
{Int32 INLINE} count
	^myTally!
*/
}
public Heaper fetch(int index) {
	int loc;
	Heaper tmp;
	loc = hashFind(index);
	tmp = myPtrs.fetch(loc);
	if (tmp == null || (tmp == PrimRemovedObject.make())) {
		return null;
	}
	return tmp;
/*
udanax-top.st:33643:PrimPtrTable methodsFor: 'accessing'!
{Heaper | NULL} fetch: index {IntegerVar}
	| loc {Int32} tmp {Heaper wimpy} |
	loc _ self hashFind: index.
	tmp _ myPtrs fetch: loc.
	(tmp == NULL or: [tmp == PrimRemovedObject make]) ifTrue:
		[ ^NULL ].
	^ tmp!
*/
}
public Heaper get(int index) {
	int loc;
	Heaper tmp;
	loc = hashFind(index);
	tmp = myPtrs.fetch(loc);
	if (tmp == null || (tmp == PrimRemovedObject.make())) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return tmp;
/*
udanax-top.st:33651:PrimPtrTable methodsFor: 'accessing'!
{Heaper} get: index {IntegerVar}
	| loc {Int32} tmp {Heaper wimpy} |
	loc _ self hashFind: index.
	tmp _ myPtrs fetch: loc.
	(tmp == NULL or: [tmp == PrimRemovedObject make]) ifTrue:
		[ Heaper BLAST: #NotInTable ].
	^ tmp!
*/
}
public void remove(int index) {
	int loc;
	loc = hashFind(index);
	if ((myPtrs.fetch(loc)) == null || ((myPtrs.fetch(loc)) == PrimRemovedObject.make())) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	myPtrs.store(loc, PrimRemovedObject.make());
	myTally = myTally - 1;
/*
udanax-top.st:33659:PrimPtrTable methodsFor: 'accessing'!
{void} remove: index {IntegerVar}
	| loc {Int32} |
	loc := self hashFind: index.
	((myPtrs fetch: loc) == NULL or: [(myPtrs fetch: loc) == PrimRemovedObject make]) ifTrue:
		[ Heaper BLAST: #NotInTable ].
	myPtrs at: loc store: PrimRemovedObject make.
	myTally := myTally - 1.!
*/
}
public void wipe(int index) {
	int loc;
	loc = hashFind(index);
	if ((myPtrs.fetch(loc)) == null || ((myPtrs.fetch(loc)) == PrimRemovedObject.make())) {
		return ;
	}
	myPtrs.store(loc, PrimRemovedObject.make());
	myTally = myTally - 1;
/*
udanax-top.st:33667:PrimPtrTable methodsFor: 'accessing'!
{void} wipe: index {IntegerVar}
	| loc {Int32} |
	loc := self hashFind: index.
	((myPtrs fetch: loc) == NULL or: [(myPtrs fetch: loc) == PrimRemovedObject make]) ifTrue: [ ^ VOID ].
	myPtrs at: loc store: PrimRemovedObject make.
	myTally := myTally - 1.!
*/
}
public void destruct() {
	myPtrs.destroy();
	myIndices.destroy();
	super.destruct();
/*
udanax-top.st:33676:PrimPtrTable methodsFor: 'protected: destruct'!
{void} destruct
	myPtrs destroy.
	myIndices destroy.
	super destruct!
*/
}
public void grow() {
	PtrArray oldPtrs;
	IntegerVarArray oldIndices;
	IntegerVarArray newIndices;
	Heaper tmp;
	Heaper removed;
	oldPtrs = myPtrs;
	oldIndices = myIndices;
	/* To be GC safe, instance variables are not modified until all allocations are complete. */
	newIndices = IntegerVarArray.zeros(5 * myPtrs.count() / 3);
	if (myExecutor == null) {
		myPtrs = PtrArray.nulls(newIndices.count());
	}
	else {
		myPtrs = WeakPtrArray.make(myExecutor, newIndices.count());
	}
	myIndices = newIndices;
	removed = PrimRemovedObject.make();
	for (int i = 0; i < oldPtrs.count(); i ++ ) {
		int loc;
		if ((tmp = oldPtrs.fetch(i)) != null && (tmp != removed)) {
			loc = hashFind((oldIndices.integerVarAt(i)));
			myIndices.storeIntegerVar(loc, (oldIndices.integerVarAt(i)));
			myPtrs.store(loc, (oldPtrs.fetch(i)));
		}
	}
	oldPtrs.destroy();
	oldIndices.destroy();
/*
udanax-top.st:33683:PrimPtrTable methodsFor: 'private:'!
{void} grow
	| oldPtrs {PtrArray} oldIndices {IntegerVarArray} newIndices {IntegerVarArray} tmp {Heaper wimpy} removed {Heaper wimpy} |
	oldPtrs := myPtrs.
	oldIndices := myIndices.
	"To be GC safe, instance variables are not modified until all allocations are complete."
	newIndices := IntegerVarArray zeros: 5 * myPtrs count // 3.
	myExecutor == NULL
		ifTrue: [myPtrs := PtrArray nulls: newIndices count]
		ifFalse: [myPtrs := WeakPtrArray make: myExecutor with: newIndices count].
	myIndices := newIndices.
	removed _ PrimRemovedObject make.
	Int32Zero almostTo: oldPtrs count do: [:i {UInt32} |
		| loc {Int32} |
		((tmp _ oldPtrs fetch: i) ~~ NULL and: [tmp ~~ removed]) ifTrue:
			[loc := self hashFind: (oldIndices integerVarAt: i).
			myIndices at: loc storeIntegerVar: (oldIndices integerVarAt: i).
			myPtrs at: loc store: (oldPtrs fetch: i)]].
	oldPtrs destroy.
	oldIndices destroy!
*/
}
public int hashFind(int value) {
	/* TODO variable may not be initialized before being used */
	int loc = 0;
	int firstRemoved;
	Heaper tmp;
	/* TODO variable may not be initialized before being used */
	Heaper removed = null;
	boolean looped;
	firstRemoved = -1;
	if ( ! (value == 0)) {
		loc = AboraSupport.modulo((FHash.fastHashUInt32(value)), myPtrs.count());
		removed = PrimRemovedObject.make();
	}
	looped = false;
	while ((tmp = myPtrs.fetch(loc)) != null) {
		if ((myIndices.integerVarAt(loc)) == value) {
			return loc;
		}
		if (tmp == removed) {
			if (firstRemoved == -1) {
				firstRemoved = loc;
			}
		}
		loc = loc + 1;
		if (loc >= myPtrs.count()) {
			if (looped) {
				return firstRemoved;
			}
			else {
				looped = true;
			}
			loc = 0;
		}
	}
	if (firstRemoved != -1) {
		return firstRemoved;
	}
	else {
		return loc;
	}
/*
udanax-top.st:33703:PrimPtrTable methodsFor: 'private:'!
{Int32} hashFind: value {IntegerVar}
	| loc {Int32}firstRemoved {Int32} tmp {Heaper wimpy} removed {Heaper wimpy} looped {BooleanVar} |
	firstRemoved _ -1.
	value == nil ifFalse: [
	loc :=  (FHash fastHash.UInt32: value DOTasLong) \\ myPtrs count.
	removed _ PrimRemovedObject make].
	looped _ false.
	[(tmp _ myPtrs fetch: loc) ~~ NULL] whileTrue:
		[(myIndices integerVarAt: loc) == value ifTrue: [ ^ loc ].
		tmp == removed ifTrue:
			[firstRemoved == -1 ifTrue: [firstRemoved _ loc]].
		loc := loc + 1.
		loc >= myPtrs count ifTrue: 
			[looped ifTrue: [^firstRemoved] ifFalse: [looped _ true].
			loc := Int32Zero]].
	firstRemoved ~~ -1
		ifTrue: [ ^ firstRemoved ]
		ifFalse: [ ^ loc ]!
*/
}
public PrimPtrTable(int size) {
	super();
	myPtrs = PtrArray.nulls(size);
	myIndices = IntegerVarArray.zeros(size);
	myTally = 0;
	myExecutor = null;
/*
udanax-top.st:33724:PrimPtrTable methodsFor: 'protected: create'!
create: size {Int32}
	super create.
	myPtrs := PtrArray nulls: size.
	myIndices := IntegerVarArray zeros: size.
	myTally := Int32Zero.
	myExecutor := NULL!
*/
}
public PrimPtrTable(int size, XnExecutor executor) {
	super();
	/* Create weak array last to be GC safe */
	myIndices = IntegerVarArray.zeros(size);
	myTally = 0;
	myExecutor = PrimPtrTableExecutor.make(this, executor);
	myPtrs = WeakPtrArray.make(myExecutor, size);
/*
udanax-top.st:33731:PrimPtrTable methodsFor: 'protected: create'!
create: size {Int32} with: executor {XnExecutor | NULL}
	super create.
	"Create weak array last to be GC safe"
	myIndices := IntegerVarArray zeros: size.
	myTally := Int32Zero.
	myExecutor := PrimPtrTableExecutor make: self with: executor.
	myPtrs := WeakPtrArray make: myExecutor with: size.!
*/
}
public PrimPtrTableStepper stepper() {
	return new PrimPtrTableStepper(myIndices, myPtrs, 0);
/*
udanax-top.st:33741:PrimPtrTable methodsFor: 'enumerating'!
{PrimPtrTableStepper} stepper
	^ PrimPtrTableStepper create: myIndices with: myPtrs with: Int32Zero!
*/
}
/**
 * By way of a weird kluge, this passes the index that the item was stored at in this table
 * to the follow up executor
 */
public void weakRemove(int index, XnExecutor follower) {
	int virtualIndex;
	myPtrs.store(index, PrimRemovedObject.make());
	virtualIndex = (myIndices.integerAt(index));
	myTally = myTally - 1;
	if (follower != null) {
		follower.execute(virtualIndex);
	}
/*
udanax-top.st:33746:PrimPtrTable methodsFor: 'private: weakness'!
{void} weakRemove: index {Int32} with: follower {XnExecutor | NULL}
	"By way of a weird kluge, this passes the index that the item was stored at in this table
	to the follow up executor"
	| virtualIndex {Int32} |
	myPtrs at: index store: PrimRemovedObject make.
	virtualIndex := (myIndices integerAt: index) DOTasLong.
	myTally := myTally - 1.
	follower ~~ NULL ifTrue:
		[follower execute: virtualIndex].!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:33758:PrimPtrTable methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static PrimPtrTable weak(int size) {
	return new PrimPtrTable(size, null);
/*
udanax-top.st:33773:PrimPtrTable class methodsFor: 'smalltalk: defaults'!
{PrimPtrTable} weak: size {Int32}
	^ self create: size with: NULL!
*/
}
public static PrimPtrTable make(int size) {
	return new PrimPtrTable(size);
/*
udanax-top.st:33778:PrimPtrTable class methodsFor: 'create'!
make: size {Int32}
	^ self create: size!
*/
}
public static PrimPtrTable weak(int size, XnExecutor executor) {
	return new PrimPtrTable(size, executor);
/*
udanax-top.st:33781:PrimPtrTable class methodsFor: 'create'!
{PrimPtrTable} weak: size {Int32} with: executor {XnExecutor default: NULL}
	^ self create: size with: executor!
*/
}
public PrimPtrTable() {
/*

Generated during transformation
*/
}
public PrimPtrTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
