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
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.primtab.PrimSetExecutor;
import info.dgjones.abora.gold.primtab.PrimSetStepper;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A set of pointers.  May be strong or weak.  If we have a separate executor, it is called
 * with the remaining size after removal.
 */
public class PrimSet extends Heaper {

	protected PtrArray myPtrs;
	protected int myTally;
	protected boolean myWeakness;
	protected XnExecutor myExecutor;
/*
udanax-top.st:33854:
Heaper subclass: #PrimSet
	instanceVariableNames: '
		myPtrs {PtrArray}
		myTally {Int4}
		myWeakness {BooleanVar}
		myExecutor {XnExecutor | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:33862:
PrimSet comment:
'A set of pointers.  May be strong or weak.  If we have a separate executor, it is called with the remaining size after removal.'!
*/
/*
udanax-top.st:33864:
(PrimSet getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimSet -/
friend class PrimSetExecutor;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:34003:
PrimSet class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34006:
(PrimSet getOrMakeCxxClassDescription)
	friends:
'/- friends for class PrimSet -/
friend class PrimSetExecutor;';
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimSet.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Stepper stepper() {
	return PrimSetStepper.make(myPtrs);
/*
udanax-top.st:33872:PrimSet methodsFor: 'enumerating'!
{Stepper} stepper
	^ PrimSetStepper make: myPtrs!
*/
}
public void introduce(Heaper value) {
	int loc;
	loc = hashFind(value);
	if (loc == -1) {
		Someone.hack();
		grow();
		loc = hashFind(value);
	}
	if ((myPtrs.fetch(loc)) == value) {
		throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_SET);
	}
	else {
		myPtrs.store(loc, value);
		myTally = myTally + 1;
		if (myTally > (2 * myPtrs.count() / 3)) {
			grow();
		}
	}
/*
udanax-top.st:33877:PrimSet methodsFor: 'adding-removing'!
{void} introduce: value {Heaper}
	| loc {Int32} |
	loc := self hashFind: value.
	loc == -1 ifTrue: [
		self hack.
		self grow.
		loc := self hashFind: value].
	(myPtrs fetch: loc) == value
		ifTrue: [ Heaper BLAST: #AlreadyInSet ]
		ifFalse: [
			myPtrs at: loc store: value.
			myTally := myTally + 1.
			myTally > (2 * myPtrs count / 3) ifTrue: [ self grow ]]!
*/
}
public void remove(Heaper value) {
	int loc;
	loc = hashFind(value);
	if ((myPtrs.fetch(loc)) != value) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SET);
	}
	myPtrs.store(loc, PrimRemovedObject.make());
	myTally = myTally - 1;
/*
udanax-top.st:33891:PrimSet methodsFor: 'adding-removing'!
{void} remove: value {Heaper}
	| loc {Int32} |
	loc := self hashFind: value.
	(myPtrs fetch: loc) ~~ value ifTrue: [ Heaper BLAST: #NotInSet ].
	myPtrs at: loc store: PrimRemovedObject make.
	myTally := myTally - 1.!
*/
}
public void store(Heaper value) {
	int loc;
	loc = hashFind(value);
	if (loc == -1) {
		Someone.hack();
		grow();
		loc = hashFind(value);
	}
	if ((myPtrs.fetch(loc)) != value) {
		myPtrs.store(loc, value);
		myTally = myTally + 1;
		if (myTally > (2 * myPtrs.count() / 3)) {
			grow();
		}
	}
/*
udanax-top.st:33898:PrimSet methodsFor: 'adding-removing'!
{void} store: value {Heaper}
	| loc {Int32} |
	loc := self hashFind: value.
	loc == -1 ifTrue: [
		self hack.
		self grow.
		loc := self hashFind: value].
	(myPtrs fetch: loc) ~= value ifTrue: [
		myPtrs at: loc store: value.
		myTally := myTally + 1.
		myTally > (2 * myPtrs count / 3) ifTrue: [ self grow ]]!
*/
}
public void wipe(Heaper value) {
	int loc;
	loc = hashFind(value);
	if ((myPtrs.fetch(loc)) == value) {
		myPtrs.store(loc, PrimRemovedObject.make());
		myTally = myTally - 1;
	}
/*
udanax-top.st:33910:PrimSet methodsFor: 'adding-removing'!
{void} wipe: value {Heaper}
	| loc {Int32} |
	loc := self hashFind: value.
	(myPtrs fetch: loc) == value ifTrue: [
		myPtrs at: loc store: PrimRemovedObject make.
		myTally := myTally - 1].!
*/
}
public void wipeAll() {
	myPtrs.storeAll();
	myTally = 0;
/*
udanax-top.st:33917:PrimSet methodsFor: 'adding-removing'!
{void} wipeAll
	myPtrs storeAll.
	myTally := Int32Zero.!
*/
}
public int count() {
	return myTally;
/*
udanax-top.st:33923:PrimSet methodsFor: 'accessing'!
{Int32 INLINE} count
	^myTally!
*/
}
public boolean hasMember(Heaper element) {
	Heaper tmp;
	tmp = myPtrs.fetch((hashFind(element)));
	return tmp == element;
/*
udanax-top.st:33926:PrimSet methodsFor: 'accessing'!
{BooleanVar} hasMember: element {Heaper}
	| tmp {Heaper wimpy} |
	tmp _ myPtrs fetch: (self hashFind: element).
	^ tmp == element!
*/
}
public boolean isEmpty() {
	return myTally == 0;
/*
udanax-top.st:33931:PrimSet methodsFor: 'accessing'!
{BooleanVar} isEmpty
	^myTally == Int32Zero!
*/
}
public void grow() {
	PtrArray oldPtrs;
	Heaper removed;
	oldPtrs = myPtrs;
	if (myWeakness) {
		myPtrs = WeakPtrArray.make((PrimSetExecutor.make(this)), 5 * oldPtrs.count() / 3);
	}
	else {
		myPtrs = PtrArray.nulls(5 * oldPtrs.count() / 3);
	}
	removed = PrimRemovedObject.make();
	for (int i = 0; i < oldPtrs.count(); i ++ ) {
		Heaper tmp;
		tmp = oldPtrs.fetch(i);
		if (tmp != null && (tmp != removed)) {
			int loc;
			loc = hashFind(tmp);
			myPtrs.store(loc, tmp);
		}
	}
	oldPtrs.destroy();
/*
udanax-top.st:33936:PrimSet methodsFor: 'private:'!
{void} grow
	| oldPtrs {PtrArray} removed {Heaper wimpy} |
	oldPtrs := myPtrs.
	myWeakness
		ifTrue: [myPtrs := WeakPtrArray make: (PrimSetExecutor make: self) with: 5 * oldPtrs count // 3]
		ifFalse: [myPtrs := PtrArray nulls: 5 * oldPtrs count // 3].
	removed := PrimRemovedObject make.
	Int32Zero almostTo: oldPtrs count do: [:i {Int32} |
		| tmp {Heaper wimpy} |
		tmp := oldPtrs fetch: i.
		(tmp ~~ NULL and: [tmp ~~ removed]) ifTrue:
			[|loc {Int32} |
			loc := self hashFind: tmp.
			myPtrs at: loc store: tmp]].
	oldPtrs destroy!
*/
}
public int hashFind(Heaper value) {
	int loc;
	int firstRemoved;
	Heaper tmp;
	Heaper removed;
	boolean looped;
	firstRemoved = -1;
	loc = value.hashForEqual();
	loc = AboraSupport.modulo((FHash.fastHashUInt32(loc)), myPtrs.count());
	removed = PrimRemovedObject.make();
	looped = false;
	while ((tmp = myPtrs.fetch(loc)) != null) {
		if (tmp == value) {
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
udanax-top.st:33952:PrimSet methodsFor: 'private:'!
{Int32} hashFind: value {Heaper}
	| loc {Int32} firstRemoved {Int32} tmp {Heaper wimpy} removed {Heaper wimpy} looped {BooleanVar} |
	firstRemoved _ -1.
	loc := value hashForEqual.
	loc :=  (FHash fastHash.UInt32: loc) \\ myPtrs count.
	removed _ PrimRemovedObject make.
	looped _ false.
	[(tmp _ myPtrs fetch: loc) ~~ NULL] whileTrue:
		[tmp == value ifTrue: [ ^ loc ].
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
public PrimSet(int size, XnExecutor exec) {
	super();
	myWeakness = true;
	myExecutor = exec;
	myPtrs = WeakPtrArray.make((PrimSetExecutor.make(this)), size);
	myTally = 0;
/*
udanax-top.st:33973:PrimSet methodsFor: 'protected: create'!
create: size {Int32} with.Executor: exec {XnExecutor}
	super create.
	myWeakness := true.
	myExecutor := exec.
	myPtrs := WeakPtrArray make: (PrimSetExecutor make: self) with: size.
	myTally := Int32Zero.!
*/
}
public PrimSet(int size, boolean weakness) {
	super();
	myWeakness = weakness;
	myExecutor = null;
	if (weakness) {
		myPtrs = WeakPtrArray.make((PrimSetExecutor.make(this)), size);
	}
	else {
		myPtrs = PtrArray.nulls(size);
	}
	myTally = 0;
/*
udanax-top.st:33980:PrimSet methodsFor: 'protected: create'!
create: size {Int32} with: weakness {BooleanVar}
	super create.
	myWeakness := weakness.
	myExecutor := NULL.
	weakness
		ifTrue: [myPtrs := WeakPtrArray make: (PrimSetExecutor make: self) with: size]
		ifFalse: [myPtrs := PtrArray nulls: size].
	myTally := Int32Zero.!
*/
}
public void weakRemove(int index) {
	myPtrs.store(index, PrimRemovedObject.make());
	/* NULL will mess up hashFind */
	myTally = myTally - 1;
	if (myExecutor != null) {
		myExecutor.execute(myTally);
	}
/*
udanax-top.st:33991:PrimSet methodsFor: 'private: weakness'!
{void} weakRemove: index {Int32}
	myPtrs at: index store: PrimRemovedObject make. "NULL will mess up hashFind"
	myTally := myTally - 1.
	myExecutor ~~ NULL ifTrue: [
		myExecutor execute: myTally].!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:33999:PrimSet methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static PrimSet make() {
	return new PrimSet(7, false);
/*
udanax-top.st:34014:PrimSet class methodsFor: 'create'!
make
	^ self create: 7 with: false!
*/
}
public static PrimSet make(int size) {
	return new PrimSet(size, false);
/*
udanax-top.st:34017:PrimSet class methodsFor: 'create'!
make: size {Int32}
	^ self create: size with: false!
*/
}
public static PrimSet weak() {
	return new PrimSet(7, true);
/*
udanax-top.st:34020:PrimSet class methodsFor: 'create'!
{PrimSet} weak
	^ self create: 7 with: true!
*/
}
public static PrimSet weak(int size) {
	return new PrimSet(size, true);
/*
udanax-top.st:34023:PrimSet class methodsFor: 'create'!
{PrimSet} weak: size {Int32}
	^ self create: size with: true!
*/
}
public static PrimSet weak(int size, XnExecutor exec) {
	return new PrimSet(size, exec);
/*
udanax-top.st:34026:PrimSet class methodsFor: 'create'!
{PrimSet} weak: size {Int32} with: exec {XnExecutor}
	^ self  create: size with.Executor: exec!
*/
}
public static PrimSet create(int size, XnExecutor exec) {
	return new PrimSet(size, exec);
/*
udanax-top.st:34031:PrimSet class methodsFor: 'smalltalk: create'!
create: size {Int32} with.Executor: exec {XnExecutor}
	^self new create: size with.Executor: exec!
*/
}
public PrimSet() {
/*

Generated during transformation
*/
}
public PrimSet(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
