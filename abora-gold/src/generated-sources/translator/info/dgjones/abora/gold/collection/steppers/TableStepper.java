/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.steppers;

import info.dgjones.abora.gold.aspire.PtrArrayAccumulator;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.cross.PtrArrayStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * For enumerating the key->value associations of a table.  A typical use (for a table whose
 * range elements were all Foos) might be:
 * SPTR(TableStepper) stomp = table->stepper();
 * FOR_EACH(Foo,f,stomp, {
 * doSomethingWith(stomp->key(), z);
 * });
 * Each iteration of the loop would correspond to an association of the table (snapshotted at
 * the time "->stepper()" was sent).  For each association, "f" (a pointer to Foo) points at
 * the range element, while "stomp->key()" provides the domain element.  See
 * ScruTable::stepper.
 */
public class TableStepper extends Stepper {

/*
udanax-top.st:55259:
Stepper subclass: #TableStepper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Steppers'!
*/
/*
udanax-top.st:55263:
TableStepper comment:
'For enumerating the key->value associations of a table.  A typical use (for a table whose range elements were all Foos) might be:
	
	SPTR(TableStepper) stomp = table->stepper();
	FOR_EACH(Foo,f,stomp, {
		doSomethingWith(stomp->key(), z);
	});
	
	Each iteration of the loop would correspond to an association of the table (snapshotted at the time "->stepper()" was sent).  For each association, "f" (a pointer to Foo) points at the range element, while "stomp->key()" provides the domain element.  See ScruTable::stepper.'!
*/
/*
udanax-top.st:55272:
(TableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:55368:
TableStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55371:
(TableStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TableStepper.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/*
udanax-top.st:55277:TableStepper methodsFor: 'smalltalk: operations'!
{void} forIndices: fn {BlockClosure of: IntegerVar with: Heaper} 
	[| result {Heaper} |
	[(result _ self fetch) ~~ NULL]
		whileTrue:
			[fn of: self index and: result.
			self step]]
		valueNowOrOnUnwindDo: [self destroy]!
*/
/*
udanax-top.st:55285:TableStepper methodsFor: 'smalltalk: operations'!
{void} forKeyValues: fn {BlockClosure of: Position with: Heaper} 
	[| result {Heaper} |
	[(result _ self fetch) ~~ NULL]
		whileTrue:
			[fn of: self position and: result.
			self step]]
		valueNowOrOnUnwindDo: [self destroy]!
*/
/*
udanax-top.st:55293:TableStepper methodsFor: 'smalltalk: operations'!
{void} forPositions: fn {BlockClosure of: Position with: Heaper} 
	[| result {Heaper} |
	[(result _ self fetch) ~~ NULL]
		whileTrue:
			[fn of: self position and: result.
			self step]]
		valueNowOrOnUnwindDo: [self destroy]!
*/
/**
 * A TableStepper actually enumerates the associations of a table. Through the normal Stepper
 * protocol, it makes available the range element of the current association. Through this
 * additional protocol, it make accessible the key of the current association.  This message
 * returns the same object as TwoStepper::other, the only difference being the static
 * knowledge that it's a Position.
 */
public Position key() {
	return position();
/*
udanax-top.st:55301:TableStepper methodsFor: 'smalltalk: operations'!
{Position} key
	"A TableStepper actually enumerates the associations of a table. Through the normal Stepper protocol, it makes available the range element of the current association. Through this additional protocol, it make accessible the key of the current association.  This message returns the same object as TwoStepper::other, the only difference being the static knowledge that it's a Position."
	^ self position!
*/
}
/**
 * Unboxed version of TableStepper::key.  See class comment in XuInteger.
 */
public int index() {
	return ((IntegerPos) position()).asIntegerVar();
/*
udanax-top.st:55308:TableStepper methodsFor: 'special'!
{IntegerVar} index
	"Unboxed version of TableStepper::key.  See class comment in XuInteger."
	
	^(self position cast: IntegerPos) asIntegerVar!
*/
}
/**
 * A TableStepper actually enumerates the associations of a table. Through the normal Stepper
 * protocol, it makes available the range element of the current association. Through this
 * additional protocol, it make accessible the key of the current association.  This message
 * returns the same object as TwoStepper::other, the only difference being the static
 * knowledge that it's a Position.
 */
public Position position() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55313:TableStepper methodsFor: 'special'!
{Position CLIENT} position
	"A TableStepper actually enumerates the associations of a table. Through the normal Stepper protocol, it makes available the range element of the current association. Through this additional protocol, it make accessible the key of the current association.  This message returns the same object as TwoStepper::other, the only difference being the static knowledge that it's a Position."
	self subclassResponsibility!
*/
}
public Stepper copy() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55320:TableStepper methodsFor: 'create'!
{Stepper} copy
	self subclassResponsibility!
*/
}
public Heaper fetch() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55325:TableStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self subclassResponsibility!
*/
}
public boolean hasValue() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55328:TableStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	self subclassResponsibility!
*/
}
public void step() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:55331:TableStepper methodsFor: 'operations'!
{void} step
	self subclassResponsibility!
*/
}
/**
 * An array of the remaining elements in alternating positions in the array
 * [k1, v1, k2, v2, k3, v3, ...]
 * Returns an array of up to count * 2 elements (or some arbitrary number if count is
 * negative), and steps the stepper the corresponding number of times. You should check
 * whether the stepper is atEnd, since it can stop before the number you give it because of
 * some internal limit or grouping issue.
 */
public PrimArray stepManyPairs(int count) {
	Accumulator result;
	int n;
	if (count >= 0) {
		n = count * 2;
	}
	else {
		n = 1000;
	}
	result = new PtrArrayAccumulator(n);
	n = 0;
	while (hasValue() && ((count < 0 && (n < 1000)) || (n < count))) {
		result.step(position());
		result.step(fetch());
		step();
		n = n + 1;
	}
	return (PrimArray) result.value();
/*
udanax-top.st:55335:TableStepper methodsFor: 'operations'!
{PrimArray CLIENT} stepManyPairs: count {Int32 default: -1}
	"An array of the remaining elements in alternating positions in the array
		[k1, v1, k2, v2, k3, v3, ...]
	Returns an array of up to count * 2 elements (or some arbitrary number if count is negative), and steps the stepper the corresponding number of times. You should check whether the stepper is atEnd, since it can stop before the number you give it because of some internal limit or grouping issue."
	
	| result {Accumulator} n {Int32} |
	count >= Int32Zero
		ifTrue: [n := count * 2]
		ifFalse: [n := 1000].
	result := PtrArrayAccumulator create: n.
	n := Int32Zero.
	[self hasValue and: [(count < Int32Zero and: [n < 1000]) or: [n < count]]] whileTrue:
		[result step: self position.
		result step: self fetch.
		self step.
		n := n + 1].
	^result value cast: PrimArray!
*/
}
/*
udanax-top.st:55355:TableStepper methodsFor: 'smalltalk: delayed iteration'!
{void} forPromisedPairs: aBlock
	self knownBug. "only works outside of a delay block"
	[self atEnd value] whileFalse:
		[aBlock value: (XuPromise dynamicType: self position) value: (XuPromise dynamicType: self get).
		self step]!
*/
public PrimArray stepManyPairs() {
	return stepManyPairs(-1);
/*
udanax-top.st:55364:TableStepper methodsFor: 'smalltalk: defaults'!
{PrimArray CLIENT} stepManyPairs
	^self stepManyPairs: -1!
*/
}
/**
 * {Position CLIENT} position
 * {PrimArray CLIENT} stepManyPairs: count {Int32 default: -1}
 */
public static void infostProtocol() {
/*
udanax-top.st:55376:TableStepper class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Position CLIENT} position
{PrimArray CLIENT} stepManyPairs: count {Int32 default: -1}
"!
*/
}
/**
 * Note: this being a low level operation, and there being no lightweight form of immutable
 * or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which
 * will in fact not be changed during the life of this stepper.  This is an unchecked an
 * uncheckable precondition on my clients.
 */
public static TableStepper ascending(PtrArray array) {
	return PtrArrayStepper.ascending(array);
/*
udanax-top.st:55383:TableStepper class methodsFor: 'creation'!
{TableStepper INLINE} ascending: array {PtrArray}
	"Note: this being a low level operation, and there being no lightweight form of immutable or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which will in fact not be changed during the life of this stepper.  This is an unchecked an uncheckable precondition on my clients."
	 
	^PtrArrayStepper ascending: array!
*/
}
/**
 * Note: this being a low level operation, and there being no lightweight form of immutable
 * or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which
 * will in fact not be changed during the life of this stepper.  This is an unchecked an
 * uncheckable precondition on my clients.
 */
public static TableStepper descending(PtrArray array) {
	return PtrArrayStepper.descending(array);
/*
udanax-top.st:55388:TableStepper class methodsFor: 'creation'!
{TableStepper INLINE} descending: array {PtrArray}
	"Note: this being a low level operation, and there being no lightweight form of immutable or lazily copied PtrArray, it is my caller's responsibility to pass me a PtrArray which will in fact not be changed during the life of this stepper.  This is an unchecked an uncheckable precondition on my clients."
	
	^PtrArrayStepper descending: array!
*/
}
public TableStepper() {
/*

Generated during transformation
*/
}
public TableStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
