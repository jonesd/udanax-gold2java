/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTable;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTableStepper;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Map wimpy pointers to strong ptrs
 */
public class PrimPtr2PtrTable extends Heaper {

	protected PtrArray myFromPtrs;
	protected PtrArray myToPtrs;
	protected int myTally;
/*
udanax-top.st:33459:
Heaper subclass: #PrimPtr2PtrTable
	instanceVariableNames: '
		myFromPtrs {PtrArray}
		myToPtrs {PtrArray}
		myTally {Int4}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:33466:
PrimPtr2PtrTable comment:
'Map wimpy pointers to strong ptrs'!
*/
/*
udanax-top.st:33468:
(PrimPtr2PtrTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimPtr2PtrTable -/
friend SPTR(PrimPtr2PtrTable)  primPtr2PtrTable (Int4 size);';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:33581:
PrimPtr2PtrTable class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33584:
(PrimPtr2PtrTable getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimPtr2PtrTable -/
friend SPTR(PrimPtr2PtrTable)  primPtr2PtrTable (Int4 size);';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPtr2PtrTable.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public PrimPtr2PtrTableStepper stepper() {
	return new PrimPtr2PtrTableStepper(myFromPtrs, myToPtrs, 0);
/*
udanax-top.st:33476:PrimPtr2PtrTable methodsFor: 'enumerating'!
{PrimPtr2PtrTableStepper} stepper
	^ PrimPtr2PtrTableStepper create: myFromPtrs with: myToPtrs with: Int32Zero!
*/
}
public void introduce(Heaper key, Heaper value) {
	int loc;
	Heaper tmp;
	loc = hashFind(key);
	if ((tmp = myToPtrs.fetch(loc)) != null && (tmp != PrimRemovedObject.make())) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
	}
	myToPtrs.store(loc, value);
	myFromPtrs.store(loc, key);
	myTally = myTally + 1;
	if (myTally > (2 * myFromPtrs.count() / 3)) {
		grow();
	}
/*
udanax-top.st:33481:PrimPtr2PtrTable methodsFor: 'accessing'!
{void} at: key {Heaper} introduce: value {Heaper}
	| loc {Int32} tmp {Heaper wimpy} |
	loc := self hashFind: key.
	((tmp _ myToPtrs fetch: loc) ~~ NULL and: [tmp ~~ PrimRemovedObject make]) ifTrue:
		[ Heaper BLAST: #AlreadyInTable ].
	myToPtrs at: loc store: value.
	myFromPtrs at: loc store: key.
	myTally := myTally + 1.
	myTally > (2 * myFromPtrs count / 3) ifTrue: [ self grow ]!
*/
}
public void store(Heaper key, Heaper value) {
	int loc;
	loc = hashFind(key);
	if ((myToPtrs.fetch(loc)) == null) {
		myTally = myTally + 1;
	}
	myToPtrs.store(loc, value);
	myFromPtrs.store(loc, key);
	if (myTally > (2 * myFromPtrs.count() / 3)) {
		grow();
	}
/*
udanax-top.st:33491:PrimPtr2PtrTable methodsFor: 'accessing'!
{void} at: key {Heaper} store: value {Heaper}
	| loc {Int32} |
	loc := self hashFind: key.
	(myToPtrs fetch: loc) == NULL ifTrue: [ myTally := myTally + 1].
	myToPtrs at: loc store: value.
	myFromPtrs at: loc store: key.
	myTally > (2 * myFromPtrs count / 3) ifTrue: [ self grow ]!
*/
}
public int count() {
	return myTally;
/*
udanax-top.st:33499:PrimPtr2PtrTable methodsFor: 'accessing'!
{Int32 INLINE} count
	^myTally!
*/
}
public Heaper fetch(Heaper key) {
	Heaper tmp;
	tmp = myToPtrs.fetch((hashFind(key)));
	if (tmp == PrimRemovedObject.make()) {
		return null;
	}
	else {
		return tmp;
	}
/*
udanax-top.st:33502:PrimPtr2PtrTable methodsFor: 'accessing'!
{Heaper} fetch: key {Heaper}
	| tmp {Heaper wimpy} |
	tmp _ myToPtrs fetch: (self hashFind: key).
	tmp == PrimRemovedObject make
		ifTrue: [  ^ NULL ]
		ifFalse: [ ^ tmp ]!
*/
}
public Heaper get(Heaper ptr) {
	Heaper result;
	if ((result = myToPtrs.fetch((hashFind(ptr)))) == null || ((result) == ((Heaper) PrimRemovedObject.make()))) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return result;
/*
udanax-top.st:33509:PrimPtr2PtrTable methodsFor: 'accessing'!
{Heaper} get: ptr {Heaper}
	| result {Heaper} |
	((result _ myToPtrs fetch: (self hashFind: ptr)) == NULL
		or: [(result basicCast: Heaper star) == (PrimRemovedObject make basicCast: Heaper star)])
		ifTrue: [ Heaper BLAST: #NotInTable ].
	^ result!
*/
}
public void remove(Heaper key) {
	int loc;
	loc = hashFind(key);
	if ((myToPtrs.fetch(loc)) == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	myToPtrs.store(loc, PrimRemovedObject.make());
	myTally = myTally - 1;
/*
udanax-top.st:33516:PrimPtr2PtrTable methodsFor: 'accessing'!
{void} remove: key {Heaper}
	| loc {Int32} |
	loc := self hashFind: key.
	(myToPtrs fetch: loc) == NULL ifTrue: [ Heaper BLAST: #NotInTable ].
	myToPtrs at: loc store: PrimRemovedObject make.
	myTally := myTally - 1.!
*/
}
public void destruct() {
	myFromPtrs.destroy();
	myToPtrs.destroy();
	super.destruct();
/*
udanax-top.st:33525:PrimPtr2PtrTable methodsFor: 'protected: destruct'!
{void} destruct
	myFromPtrs destroy.
	myToPtrs destroy.
	super destruct!
*/
}
public PrimPtr2PtrTable(int size) {
	super();
	myFromPtrs = WeakPtrArray.make(XnExecutor.noopExecutor(), size);
	myToPtrs = PtrArray.nulls(size);
	myTally = 0;
/*
udanax-top.st:33532:PrimPtr2PtrTable methodsFor: 'protected: create'!
create: size {Int32}
	super create.
	myFromPtrs := WeakPtrArray make: XnExecutor noopExecutor with: size.
	myToPtrs := PtrArray nulls: size.
	myTally := Int32Zero.!
*/
}
public void grow() {
	PtrArray oldFromPtrs;
	PtrArray oldToPtrs;
	Heaper tmp;
	Heaper removed;
	oldFromPtrs = myFromPtrs;
	oldToPtrs = myToPtrs;
	myFromPtrs = WeakPtrArray.make(XnExecutor.noopExecutor(), 5 * myFromPtrs.count() / 3);
	myToPtrs = PtrArray.nulls(myFromPtrs.count());
	removed = PrimRemovedObject.make();
	for (int i = 0; i < oldFromPtrs.count(); i ++ ) {
		int loc;
		if ((tmp = oldToPtrs.fetch(i)) != null && (tmp != removed)) {
			loc = hashFind((oldFromPtrs.fetch(i)));
			myFromPtrs.store(loc, (oldFromPtrs.fetch(i)));
			myToPtrs.store(loc, (oldToPtrs.fetch(i)));
		}
	}
	oldFromPtrs.destroy();
	oldToPtrs.destroy();
/*
udanax-top.st:33540:PrimPtr2PtrTable methodsFor: 'private:'!
{void} grow
	| oldFromPtrs {PtrArray} oldToPtrs {PtrArray} tmp {Heaper wimpy} removed {Heaper wimpy} |
	oldFromPtrs := myFromPtrs.
	oldToPtrs := myToPtrs.
	myFromPtrs := WeakPtrArray make: XnExecutor noopExecutor with: 5 * myFromPtrs count // 3.
	myToPtrs := PtrArray nulls: myFromPtrs count.
	removed _ PrimRemovedObject make.
	Int32Zero almostTo: oldFromPtrs count do: [:i {Int32} |
		| loc {Int32} |
		((tmp _ oldToPtrs fetch: i) ~~ NULL and: [tmp ~~ removed]) ifTrue:
			[loc := self hashFind: (oldFromPtrs fetch: i).
			myFromPtrs at: loc store: (oldFromPtrs fetch: i).
			myToPtrs at: loc store: (oldToPtrs fetch: i)]].
	oldFromPtrs destroy.
	oldToPtrs destroy!
*/
}
public int hashFind(Heaper key) {
	int loc;
	int firstRemoved;
	Heaper tmp;
	Heaper removed;
	boolean looped;
	firstRemoved = -1;
	loc = key.hashForEqual();
	loc = AboraSupport.modulo((FHash.fastHashUInt32(loc)), myFromPtrs.count());
	removed = PrimRemovedObject.make();
	looped = false;
	while ((tmp = myToPtrs.fetch(loc)) != null) {
		if (((Heaper) (myFromPtrs.fetch(loc))) == key) {
			return loc;
		}
		if (tmp == removed) {
			if (firstRemoved == -1) {
				firstRemoved = loc;
			}
		}
		loc = loc + 1;
		if (loc >= myFromPtrs.count()) {
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
udanax-top.st:33556:PrimPtr2PtrTable methodsFor: 'private:'!
{Int32} hashFind: key {Heaper}
	| loc {Int32} firstRemoved {Int32} tmp {Heaper wimpy} removed {Heaper wimpy} looped {BooleanVar} |
	firstRemoved _ -1.
	loc := key hashForEqual.
	loc :=  (FHash fastHash.UInt32: loc) \\ myFromPtrs count.
	removed _ PrimRemovedObject make.
	looped _ false.
	[(tmp _ myToPtrs fetch: loc) ~~ NULL] whileTrue:
		[((myFromPtrs fetch: loc) basicCast: Heaper star) == key ifTrue: [ ^ loc ].
		tmp == removed ifTrue:
			[firstRemoved == -1 ifTrue: [firstRemoved _ loc]].
		loc := loc + 1.
		loc >= myFromPtrs count ifTrue: 
			[looped ifTrue: [^firstRemoved] ifFalse: [looped _ true].
			loc := Int32Zero]].
	firstRemoved ~~ -1
		ifTrue: [ ^ firstRemoved ]
		ifFalse: [ ^ loc ]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:33577:PrimPtr2PtrTable methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static PrimPtr2PtrTable make(int size) {
	return new PrimPtr2PtrTable(size);
/*
udanax-top.st:33592:PrimPtr2PtrTable class methodsFor: 'create'!
make: size {Int32}
	^ self create: size!
*/
}
public PrimPtr2PtrTable() {
/*

Generated during transformation
*/
}
public PrimPtr2PtrTable(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
