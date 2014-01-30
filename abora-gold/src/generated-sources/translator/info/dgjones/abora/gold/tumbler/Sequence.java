/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tumbler.AfterSequence;
import info.dgjones.abora.gold.tumbler.BeforeSequence;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.x.PrimIntegerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Represents an infinite sequence of integers (of which only a finite number can be
 * non-zero). They are lexically ordered, and there is a "decimal point" between the numbers
 * at -1 and 0.
 * Implementation note:
 * The array should have no zeros at either end, and noone else should have a pointer to it.
 */
public class Sequence extends Position {

	protected int myShift;
	protected PrimIntegerArray myNumbers;
	protected static Sequence TheZero;
/*
udanax-top.st:32314:
Position subclass: #Sequence
	instanceVariableNames: '
		myShift {IntegerVar}
		myNumbers {PrimIntegerArray}'
	classVariableNames: 'TheZero {Sequence} '
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:32320:
Sequence comment:
'Represents an infinite sequence of integers (of which only a finite number can be non-zero). They are lexically ordered, and there is a "decimal point" between the numbers at -1 and 0.
Implementation note:
The array should have no zeros at either end, and noone else should have a pointer to it.'!
*/
/*
udanax-top.st:32325:
(Sequence getOrMakeCxxClassDescription)
	friends:
'/- friends for class Sequence -/
friend class AfterSequence;
friend class BeforeSequence;
friend class BeforeSequencePrefix;
friend class SequenceUpOrder;
friend class SequenceSpace;';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:32625:
Sequence class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:32628:
(Sequence getOrMakeCxxClassDescription)
	friends:
'/- friends for class Sequence -/
friend class AfterSequence;
friend class BeforeSequence;
friend class BeforeSequencePrefix;
friend class SequenceUpOrder;
friend class SequenceSpace;';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Sequence.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion asRegion() {
	return SequenceRegion.usingx(false, ((PtrArray) (PrimSpec.pointer().arrayWithTwo((BeforeSequence.make(this)), (AfterSequence.make(this))))));
/*
udanax-top.st:32337:Sequence methodsFor: 'accessing'!
{XnRegion} asRegion
	^SequenceRegion usingx: false
		with: ((PrimSpec pointer
			arrayWithTwo: (BeforeSequence make: self)
			with: (AfterSequence make: self)) cast: PtrArray)!
*/
}
public CoordinateSpace coordinateSpace() {
	return SequenceSpace.make();
/*
udanax-top.st:32344:Sequence methodsFor: 'accessing'!
{CoordinateSpace INLINE} coordinateSpace
	^SequenceSpace make!
*/
}
/**
 * How many numbers in the sequence, not counting leading or trailing zeros
 */
public int count() {
	return myNumbers.count();
/*
udanax-top.st:32348:Sequence methodsFor: 'accessing'!
{IntegerVar INLINE} count
	"How many numbers in the sequence, not counting leading or trailing zeros"
	
	^myNumbers count!
*/
}
/**
 * The smallest index with a non-zero number. Blasts if it is all zeros.
 */
public int firstIndex() {
	if (myNumbers.count() == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.ZERO_SEQUENCE);
	}
	return myShift;
/*
udanax-top.st:32353:Sequence methodsFor: 'accessing'!
{IntegerVar CLIENT} firstIndex
	"The smallest index with a non-zero number. Blasts if it is all zeros."
	
	myNumbers count = Int32Zero ifTrue:
		[Heaper BLAST: #ZeroSequence].
	^myShift!
*/
}
/**
 * The number at the given index in the Sequence. Returns zeros beyond either end of the
 * array.
 */
public int integerAt(int index) {
	int i;
	i = index - myShift;
	if (i >= 0 && (i < count())) {
		return myNumbers.integerAt(i);
	}
	else {
		return 0;
	}
/*
udanax-top.st:32360:Sequence methodsFor: 'accessing'!
{IntegerVar CLIENT} integerAt: index {IntegerVar}
	"The number at the given index in the Sequence. Returns zeros beyond either end of the array."
	
	| i {IntegerVar} |
	i := index - myShift.
	(i >= IntegerVarZero and: [i < self count]) ifTrue:
		[^myNumbers integerAt: i DOTasLong]
	ifFalse:
		[^IntegerVarZero]!
*/
}
/**
 * Essential. The numbers in this Sequence. This is a copy of the array, so you may modify
 * it.
 * Note that two Sequences which are isEqual, may actually have arrays of numbers which have
 * different specs. Also, the array will not have any zeros at the beginning or end.
 */
public PrimIntegerArray integers() {
	return (PrimIntegerArray) myNumbers.copy();
/*
udanax-top.st:32370:Sequence methodsFor: 'accessing'!
{PrimIntegerArray CLIENT} integers
	"Essential. The numbers in this Sequence. This is a copy of the array, so you may modify it.
	Note that two Sequences which are isEqual, may actually have arrays of numbers which have different specs. Also, the array will not have any zeros at the beginning or end."
	
	^myNumbers copy cast: PrimIntegerArray!
*/
}
/**
 * Whether all the numbers in the sequence are zero
 */
public boolean isZero() {
	return myNumbers.count() == 0;
/*
udanax-top.st:32376:Sequence methodsFor: 'accessing'!
{BooleanVar CLIENT} isZero
	"Whether all the numbers in the sequence are zero"
	
	^myNumbers count == Int32Zero!
*/
}
/**
 * The largest index with a non-zero number. Blasts if it is all zeros.
 */
public int lastIndex() {
	if (myNumbers.count() == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.ZERO_SEQUENCE);
	}
	return myShift + myNumbers.count() - 1;
/*
udanax-top.st:32381:Sequence methodsFor: 'accessing'!
{IntegerVar CLIENT} lastIndex
	"The largest index with a non-zero number. Blasts if it is all zeros."
	
	myNumbers count = Int32Zero ifTrue:
		[Heaper BLAST: #ZeroSequence].
	^myShift + myNumbers count - 1!
*/
}
/**
 * The amount by which the numbers are shifted. Positive means less significant, negative
 * means more significant. This is contrary to the usual arithmetic notions, but it is the
 * right thing for arrays.
 */
public int shift() {
	return myShift;
/*
udanax-top.st:32388:Sequence methodsFor: 'accessing'!
{IntegerVar INLINE} shift
	"The amount by which the numbers are shifted. Positive means less significant, negative means more significant. This is contrary to the usual arithmetic notions, but it is the right thing for arrays."
	
	^myShift!
*/
}
/**
 * Compare my numbers up to and including index n with the corresponding numbers in the other
 * Sequence. Return -1, 0 or 1 depending on whether they are <, =, or > the other.
 */
public int comparePrefix(Sequence other, int n) {
	int diff;
	if (isZero() || (myShift > n)) {
		if (other.isZero() || (other.shift() > n)) {
			return 0;
		}
		if ((other.secretNumbers().integerAt(0)) > 0) {
			return -1;
		}
		else {
			return 1;
		}
	}
	if (other.isZero() || (other.shift() > n)) {
		if ((myNumbers.integerAt(0)) > 0) {
			return 1;
		}
		else {
			return -1;
		}
	}
	diff = myShift - other.shift();
	if (diff < 0) {
		if ((myNumbers.integerAt(0)) > 0) {
			return 1;
		}
		else {
			return -1;
		}
	}
	if (diff > 0) {
		if ((other.secretNumbers().integerAt(0)) > 0) {
			return -1;
		}
		else {
			return 1;
		}
	}
	return myNumbers.compare(other.secretNumbers(), (Math.min(n - myShift + 1, (Math.max(myNumbers.count(), other.secretNumbers().count())))));
/*
udanax-top.st:32395:Sequence methodsFor: 'private: comparing'!
{Int32} comparePrefix: other {Sequence} with: n {IntegerVar}
	"Compare my numbers up to and including index n with the corresponding numbers in the other Sequence. Return -1, 0 or 1 depending on whether they are <, =, or > the other."
	
	| diff {IntegerVar} |
	(self isZero or: [myShift > n]) ifTrue:
		[(other isZero or: [other shift > n]) ifTrue: [^Int32Zero].
		(other secretNumbers integerAt: Int32Zero) > IntegerVarZero
			ifTrue: [^-1]
			ifFalse: [^1]].
	(other isZero or: [other shift > n]) ifTrue:
		[(myNumbers integerAt: Int32Zero) > IntegerVarZero
			ifTrue: [^1]
			ifFalse: [^-1]].
	diff := myShift - other shift.
	diff < IntegerVarZero ifTrue:
		[(myNumbers integerAt: Int32Zero) > IntegerVarZero
			ifTrue: [^1]
			ifFalse: [^-1]].
	diff > IntegerVarZero ifTrue:
		[(other secretNumbers integerAt: Int32Zero) > IntegerVarZero
			ifTrue: [^-1]
			ifFalse: [^1]].
	^myNumbers compare: other secretNumbers
		with: (n - myShift + 1 min: (myNumbers count max: other secretNumbers count)) DOTasLong!
*/
}
public int actualHashForEqual() {
	return myShift ^ myNumbers.elementsHash();
/*
udanax-top.st:32422:Sequence methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myShift DOTasLong bitXor: myNumbers elementsHash!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof Sequence) {
		Sequence sequence = (Sequence) other;
		return myShift == sequence.shift() && (myNumbers.contentsEqual(sequence.secretNumbers()));
	}
	else {
		return false;
	}
/*
udanax-top.st:32426:Sequence methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: Sequence into: [ :sequence |
		^myShift = sequence shift
			and: [myNumbers contentsEqual: sequence secretNumbers]]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
/**
 * Whether this sequence is greater than or equal to the other sequence, using a lexical
 * comparison of their corresponding numbers.
 */
public boolean isGE(Position other) {
	Sequence o;
	o = (Sequence) other;
	if (isZero()) {
		return o.isZero() || ((o.secretNumbers().integerAt(0)) <= 0);
	}
	if (o.isZero() || (myShift < o.shift())) {
		return isZero() || ((myNumbers.integerAt(0)) >= 0);
	}
	if (myShift > o.shift()) {
		return (o.secretNumbers().integerAt(0)) <= 0;
	}
	if (myShift < o.shift()) {
		return (myNumbers.integerAt(0)) >= 0;
	}
	return (myNumbers.compare(o.secretNumbers())) >= 0;
/*
udanax-top.st:32435:Sequence methodsFor: 'testing'!
{BooleanVar} isGE: other {Position}
	"Whether this sequence is greater than or equal to the other sequence, using a lexical comparison of their corresponding numbers."
	
	| o {Sequence} |
	o _ other cast: Sequence.
	(self isZero) ifTrue:
		[^o isZero
			or: [(o secretNumbers integerAt: Int32Zero) <= IntegerVarZero]].
	(o isZero or: [myShift < o shift]) ifTrue:
		[^self isZero
			or: [(myNumbers integerAt: Int32Zero) >= IntegerVarZero]].
	myShift > o shift ifTrue:
		[^(o secretNumbers integerAt: Int32Zero) <= IntegerVarZero].
	myShift < o shift ifTrue:
		[^(myNumbers integerAt: Int32Zero) >= IntegerVarZero].
	^(myNumbers compare: o secretNumbers) >= Int32Zero!
*/
}
/**
 * The array itself, for internal use
 */
public PrimIntegerArray secretNumbers() {
	return myNumbers;
/*
udanax-top.st:32454:Sequence methodsFor: 'private:'!
{PrimIntegerArray INLINE} secretNumbers
	"The array itself, for internal use"
	
	^myNumbers!
*/
}
public void printOn(PrintWriter oo) {
	Sequence.printOn(oo, myShift, myNumbers);
/*
udanax-top.st:32461:Sequence methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	Sequence printOn: oo with: myShift with: myNumbers!
*/
}
public Sequence(int shift, PrimIntegerArray numbers) {
	super();
	myShift = shift;
	myNumbers = numbers;
/*
udanax-top.st:32467:Sequence methodsFor: 'create'!
create: shift {IntegerVar} with: numbers {PrimIntegerArray}
	super create.
	myShift := shift.
	myNumbers := numbers.!
*/
}
/**
 * The sequence consisting of all numbers in this one up to but not including the first zero,
 * or the entire thing if there are no zeros
 */
public Sequence first() {
	/* | zero {Int32} |
	zero := myNumbers indexOfInteger: IntegerVarZero.
	zero < Int32Zero ifTrue:
		[^self]
	ifFalse:
		[^Sequence create: ((myNumbers copy: zero) cast: PrimIntegerArray)] */
	Someone.shouldImplement();
	return null;
/*
udanax-top.st:32475:Sequence methodsFor: 'operations'!
{Sequence} first
	"The sequence consisting of all numbers in this one up to but not including the first zero, or the entire thing if there are no zeros"
	
	"| zero {Int32} |
	zero := myNumbers indexOfInteger: IntegerVarZero.
	zero < Int32Zero ifTrue:
		[^self]
	ifFalse:
		[^Sequence create: ((myNumbers copy: zero) cast: PrimIntegerArray)]"
	Someone shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * A sequence with the corresponding numbers subtracted from each other
 */
public Sequence minus(Sequence other) {
	int diff;
	PrimIntegerArray result;
	Ravi.thingToDo();
	/* Only increase representation size when necessary */
	Ravi.knownBug();
	/* large difference in shifts creates huge array */
	diff = (other.shift() - myShift);
	if (diff > 0) {
		result = (PrimIntegerArray) (PrimSpec.integerVar().copyGrow(myNumbers, (Math.max(diff + other.secretNumbers().count() - myNumbers.count(), 0))));
		result.subtractElements(diff, other.secretNumbers());
		return Sequence.usingx(myShift, result);
	}
	else {
		result = (PrimIntegerArray) (PrimSpec.integerVar().copy(myNumbers, -1, 0, - diff, (Math.max((other.shift() + other.count() - (myShift + myNumbers.count())), 0))));
		result.subtractElements( - diff, other.secretNumbers());
		return Sequence.usingx(other.shift(), result);
	}
/*
udanax-top.st:32487:Sequence methodsFor: 'operations'!
{Sequence} minus: other {Sequence}
	"A sequence with the corresponding numbers subtracted from each other"
	
	| diff {Int32} result {PrimIntegerArray} |
	Ravi thingToDo. "Only increase representation size when necessary"
	Ravi knownBug. "large difference in shifts creates huge array"
	diff := (other shift - myShift) DOTasLong.
	diff > Int32Zero ifTrue:
		[result := (PrimSpec integerVar copyGrow: myNumbers
			with: (diff + other secretNumbers count - myNumbers count max: Int32Zero)) cast: PrimIntegerArray.
		result at: diff subtractElements: other secretNumbers.
		^Sequence usingx: myShift with: result]
	ifFalse:
		[result := (PrimSpec integerVar copy: myNumbers
			with: -1
			with: Int32Zero
			with: diff negated
			with: ((other shift + other count - (myShift + myNumbers count)) DOTasLong max: Int32Zero)) cast: PrimIntegerArray.
		result at: diff negated subtractElements: other secretNumbers.
		^Sequence usingx: other shift with: result]!
*/
}
/**
 * A sequence with the corresponding numbers added to each other
 */
public Sequence plus(Sequence other) {
	int diff;
	PrimIntegerArray result;
	Ravi.thingToDo();
	/* Only increase representation size when necessary */
	Ravi.knownBug();
	/* large difference in shifts creates huge array */
	diff = (other.shift() - myShift);
	if (diff > 0) {
		result = (PrimIntegerArray) (PrimSpec.integerVar().copyGrow(myNumbers, (Math.max(diff + other.secretNumbers().count() - myNumbers.count(), 0))));
		result.addElements(diff, other.secretNumbers());
		return Sequence.usingx(myShift, result);
	}
	else {
		result = (PrimIntegerArray) (PrimSpec.integerVar().copy(myNumbers, -1, 0, - diff, (Math.max((other.shift() + other.count() - (myShift + myNumbers.count())), 0))));
		result.addElements(0, other.secretNumbers());
		return Sequence.usingx(other.shift(), result);
	}
/*
udanax-top.st:32508:Sequence methodsFor: 'operations'!
{Sequence} plus: other {Sequence}
	"A sequence with the corresponding numbers added to each other"
	
	| diff {Int32} result {PrimIntegerArray} |
	Ravi thingToDo. "Only increase representation size when necessary"
	Ravi knownBug. "large difference in shifts creates huge array"
	diff := (other shift - myShift) DOTasLong.
	diff > Int32Zero ifTrue:
		[result := (PrimSpec integerVar copyGrow: myNumbers
			with: (diff + other secretNumbers count - myNumbers count max: Int32Zero)) cast: PrimIntegerArray.
		result at: diff addElements: other secretNumbers.
		^Sequence usingx: myShift with: result]
	ifFalse:
		[result := (PrimSpec integerVar copy: myNumbers
			with: -1
			with: Int32Zero
			with: diff negated
			with: ((other shift + other count - (myShift + myNumbers count)) DOTasLong max: Int32Zero)) cast: PrimIntegerArray.
		result at: Int32Zero addElements: other secretNumbers.
		^Sequence usingx: other shift with: result]!
*/
}
/**
 * The sequence consisting of all numbers in this one after but not including the first zero,
 * or a null sequence if there are no zeros
 */
public Sequence rest() {
	/* | zero {Int32} |
	zero := myNumbers indexOfInteger: IntegerVarZero.
	zero < Int32Zero ifTrue:
		[^Sequence zero]
	ifFalse:
		[^Sequence create: ((myNumbers
			copy: -1 with: 1 + zero) cast: PrimIntegerArray)] */
	Someone.shouldImplement();
	return null;
/*
udanax-top.st:32529:Sequence methodsFor: 'operations'!
{Sequence} rest
	"The sequence consisting of all numbers in this one after but not including the first zero, or a null sequence if there are no zeros"
	
	"| zero {Int32} |
	zero := myNumbers indexOfInteger: IntegerVarZero.
	zero < Int32Zero ifTrue:
		[^Sequence zero]
	ifFalse:
		[^Sequence create: ((myNumbers
			copy: -1 with: 1 + zero) cast: PrimIntegerArray)]"
	Someone shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * Shift the numbers by some number of places. Positive shifts make it less significant,
 * negative shifts make it more significant.
 */
public Sequence shift(int offset) {
	if (offset == 0 || (myNumbers.count() == 0)) {
		return this;
	}
	return new Sequence(myShift + offset, myNumbers);
/*
udanax-top.st:32542:Sequence methodsFor: 'operations'!
{Sequence} shift: offset {IntegerVar}
	"Shift the numbers by some number of places. Positive shifts make it less significant, negative shifts make it more significant."
	
	(offset == IntegerVarZero or: [myNumbers count == Int32Zero]) ifTrue: [^self].
	^Sequence create: myShift + offset
		with: myNumbers!
*/
}
/**
 * Change a single element of the sequence.
 */
public Sequence with(int index, int number) {
	if (index >= myShift && (index - myShift < myNumbers.count())) {
		if (number == 0) {
			if (index == myShift) {
				return new Sequence(myShift + 1, ((PrimIntegerArray) (myNumbers.copy(myNumbers.count() - 1, 1))));
			}
			if (index == (myShift + myNumbers.count())) {
				return new Sequence(myShift + 1, ((PrimIntegerArray) (myNumbers.copy(myNumbers.count() - 1))));
			}
		}
		return new Sequence(myShift, (myNumbers.hold((index - myShift), number)));
	}
	if (number == 0) {
		return this;
	}
	if (index < myShift) {
		PrimIntegerArray result;
		result = (PrimIntegerArray) ((((PrimIntegerSpec) myNumbers.spec()).combine(((PrimIntegerSpec) (PrimSpec.toHold(number))))).copy(myNumbers, -1, 0, (myShift - index)));
		result.storeInteger(0, number);
		return new Sequence(index, result);
	}
	return new Sequence(myShift, (myNumbers.hold((index - myShift), number)));
/*
udanax-top.st:32549:Sequence methodsFor: 'operations'!
{Sequence CLIENT} with: index {IntegerVar} with: number {IntegerVar}
	"Change a single element of the sequence."
	
	(index >= myShift and: [index - myShift < myNumbers count]) ifTrue:
		[number = IntegerVarZero ifTrue:
			[index = myShift ifTrue:
				[^Sequence create: myShift + 1
					with: ((myNumbers copy: myNumbers count - 1 with: 1) cast: PrimIntegerArray)].
			index = (myShift + myNumbers count) ifTrue:
				[^Sequence create: myShift + 1
					with: ((myNumbers copy: myNumbers count - 1) cast: PrimIntegerArray)]].
		^Sequence create: myShift
			with: (myNumbers at: (index - myShift) DOTasLong hold: number)].
	number = IntegerVarZero ifTrue:
		[^self].
	index < myShift ifTrue:
		[ | result {PrimIntegerArray} |
		result := (((myNumbers spec cast: PrimIntegerSpec) combine: ((PrimSpec toHold: number) cast: PrimIntegerSpec))
			copy: myNumbers
			with: -1
			with: Int32Zero
			with: (myShift - index) DOTasLong) cast: PrimIntegerArray.
		result at: Int32Zero storeInteger: number.
		^Sequence create: index with: result].
	^Sequence create: myShift
		with: (myNumbers at: (index - myShift) DOTasLong hold: number)!
*/
}
/**
 * A Sequence with all my numbers followed by the given one
 */
public Sequence withFirst(int number) {
	Ravi.shouldImplement();
	return null;
/*
udanax-top.st:32576:Sequence methodsFor: 'operations'!
{Sequence} withFirst: number {IntegerVar}
	"A Sequence with all my numbers followed by the given one"
	
	Ravi shouldImplement.
	^NULL "fodder"!
*/
}
/**
 * A Sequence with all my numbers followed by the given one
 */
public Sequence withLast(int number) {
	return new Sequence(myShift, (myNumbers.hold(myNumbers.count(), number)));
/*
udanax-top.st:32582:Sequence methodsFor: 'operations'!
{Sequence} withLast: number {IntegerVar}
	"A Sequence with all my numbers followed by the given one"
	
	^Sequence create: myShift
		with: (myNumbers at: myNumbers count hold: number)!
*/
}
/**
 * A sequence containing all the numbers in this one, followed by the other one, separated by
 * a single zero.
 */
public Sequence withRest(Sequence other) {
	PrimIntegerSpec spec;
	PrimIntegerArray result;
	spec = ((PrimIntegerSpec) myNumbers.spec()).combine(((PrimIntegerSpec) other.secretNumbers().spec()));
	result = (PrimIntegerArray) (spec.copyGrow(myNumbers, other.count() + 1));
	result.storeMany(count() + 1, other.secretNumbers());
	return new Sequence(myShift, result);
/*
udanax-top.st:32588:Sequence methodsFor: 'operations'!
{Sequence} withRest: other {Sequence}
	"A sequence containing all the numbers in this one, followed by the other one, separated by a single zero."
	
	| spec {PrimIntegerSpec} result {PrimIntegerArray} |
	spec := (myNumbers spec cast: PrimIntegerSpec) combine: (other secretNumbers spec cast: PrimIntegerSpec).
	result := (spec copyGrow: myNumbers with: other count DOTasLong + 1) cast: PrimIntegerArray.
	result at: self count DOTasLong + 1 storeMany: other secretNumbers.
	^Sequence create: myShift with: result!
*/
}
/**
 * Whether there are no non-zero numbers in the Sequence
 * @deprecated
 */
public boolean isEmpty() {
	throw new PasseException();
/*
udanax-top.st:32599:Sequence methodsFor: 'smalltalk: passe'!
{BooleanVar} isEmpty
	"Whether there are no non-zero numbers in the Sequence"
	self passe.
	^myNumbers count == Int32Zero!
*/
}
/**
 * @deprecated
 */
public int numberAt(int index) {
	throw new PasseException();
/*
udanax-top.st:32604:Sequence methodsFor: 'smalltalk: passe'!
{IntegerVar} numberAt: index {IntegerVar}
	self passe "integerAt"!
*/
}
/**
 * @deprecated
 */
public PrimIntegerArray numbers() {
	throw new PasseException();
/*
udanax-top.st:32608:Sequence methodsFor: 'smalltalk: passe'!
{PrimIntegerArray} numbers
	self passe. "integers"!
*/
}
public Sequence(Rcvr receiver) {
	super(receiver);
	myShift = receiver.receiveIntegerVar();
	myNumbers = (PrimIntegerArray) receiver.receiveHeaper();
/*
udanax-top.st:32614:Sequence methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myShift _ receiver receiveIntegerVar.
	myNumbers _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myShift);
	xmtr.sendHeaper(myNumbers);
/*
udanax-top.st:32619:Sequence methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myShift.
	xmtr sendHeaper: myNumbers.!
*/
}
public static Sequence numbers(PrimIntegerArray digits) {
	int first;
	int last;
	first = digits.indexPastInteger(0);
	if (first == -1) {
		return Sequence.zero();
	}
	last = digits.indexPastInteger(0, -1, -1);
	return new Sequence(first, ((PrimIntegerArray) (digits.copy(last - first + 1, first))));
/*
udanax-top.st:32640:Sequence class methodsFor: 'pseudo constructors'!
{Sequence} numbers: digits {PrimIntegerArray}
 
	|first {Int32} last {Int32} |
	first := digits indexPastInteger: IntegerVarZero.
	first = -1 ifTrue: [^ Sequence zero].
	last := digits indexPastInteger: IntegerVarZero with: -1 with: -1.
	^ self create: first with: ((digits copy: last - first + 1 with: first) cast: PrimIntegerArray)!
*/
}
/**
 * A single element Sequence
 */
public static Sequence one(int a) {
	if (a == 0) {
		return zero();
	}
	return new Sequence(0, ((PrimIntegerArray) (PrimSpec.integerVar().arrayWith((PrimSpec.integerVar().value(a))))));
/*
udanax-top.st:32648:Sequence class methodsFor: 'pseudo constructors'!
{Sequence} one: a {IntegerVar}
	"A single element Sequence"
	
	a = IntegerVarZero ifTrue:
		[^self zero].
	^self create: IntegerVarZero with: ((PrimSpec integerVar arrayWith: (PrimSpec integerVar value: a)) cast: PrimIntegerArray)!
*/
}
public static Sequence string(String string) {
	return new Sequence(0, (UInt8Array.string(string)));
/*
udanax-top.st:32655:Sequence class methodsFor: 'pseudo constructors'!
{Sequence} string: string {Character star}
	^self create: IntegerVarZero with: (UInt8Array string: string)!
*/
}
/**
 * A three element Sequence
 */
public static Sequence three(int a, int b, int c) {
	if (c == 0) {
		return two(a, b);
	}
	return new Sequence(0, ((PrimIntegerArray) (PrimSpec.integerVar().arrayWithThree((PrimSpec.integerVar().value(a)), (PrimSpec.integerVar().value(b)), (PrimSpec.integerVar().value(c))))));
/*
udanax-top.st:32659:Sequence class methodsFor: 'pseudo constructors'!
{Sequence} three: a {IntegerVar} with: b {IntegerVar} with: c {IntegerVar}
	"A three element Sequence"
	
	c = IntegerVarZero ifTrue:
		[^self two: a with: b].
	^self create: IntegerVarZero
		  with: ((PrimSpec integerVar arrayWithThree: (PrimSpec integerVar value: a)
		  										 with: (PrimSpec integerVar value: b) 
		  										 with: (PrimSpec integerVar value: c)) cast: PrimIntegerArray)!
*/
}
/**
 * A two element Sequence
 */
public static Sequence two(int a, int b) {
	if (b == 0) {
		return one(a);
	}
	return new Sequence(0, ((PrimIntegerArray) (PrimSpec.integerVar().arrayWithTwo((PrimSpec.integerVar().value(a)), (PrimSpec.integerVar().value(b))))));
/*
udanax-top.st:32669:Sequence class methodsFor: 'pseudo constructors'!
{Sequence} two: a {IntegerVar} with: b {IntegerVar}
	"A two element Sequence"
	
	b = IntegerVarZero ifTrue:
		[^self one: a].
	^self create: IntegerVarZero
		    with: ((PrimSpec integerVar arrayWithTwo: (PrimSpec integerVar value: a)
		    										  with: (PrimSpec integerVar value: b)) cast: PrimIntegerArray)!
*/
}
public static Sequence zero() {
	return TheZero;
/*
udanax-top.st:32678:Sequence class methodsFor: 'pseudo constructors'!
{Sequence INLINE} zero
	^TheZero!
*/
}
/**
 * Print a sequence of numbers separated by dots. Deal with strings specially.
 */
public static void printArrayOn(PrintWriter oo, PrimIntegerArray numbers) {
	if (numbers instanceof UInt8Array) {
		oo.print("<");
		oo.print(numbers);
		oo.print(">");
	}
	else {
		for (int i = 0; i < numbers.count(); i ++ ) {
			if (i > 0) {
				oo.print(".");
			}
			oo.print((numbers.integerAt(i)));
		}
	}
/*
udanax-top.st:32684:Sequence class methodsFor: 'private:'!
{void} printArrayOn: oo {ostream reference} with: numbers {PrimIntegerArray}
	"Print a sequence of numbers separated by dots. Deal with strings specially."
	
	(numbers isKindOf: UInt8Array) ifTrue:
		[oo << '<' << numbers << '>']
	ifFalse:
		[Int32Zero almostTo: numbers count do: [ :i {Int32} |
			i > Int32Zero ifTrue:
				[oo << '.'].
			oo << (numbers integerAt: i)]]!
*/
}
/**
 * Print a sequence of numbers separated by dots. Deal with strings specially.
 */
public static void printOn(PrintWriter oo, int shift, PrimIntegerArray numbers) {
	if (shift < - numbers.count()) {
		printArrayOn(oo, numbers);
		oo.print(".");
		printZerosOn(oo, - shift - numbers.count());
		oo.print("!0");
	}
	else {
		if (shift < 0) {
			printArrayOn(oo, ((PrimIntegerArray) (numbers.copy( - shift))));
			oo.print("!");
			printArrayOn(oo, ((PrimIntegerArray) (numbers.copy(-1, - shift))));
		}
		else {
			oo.print("0!");
			if (shift > 0) {
				printZerosOn(oo, shift);
				oo.print(".");
			}
			printArrayOn(oo, numbers);
		}
	}
/*
udanax-top.st:32695:Sequence class methodsFor: 'private:'!
{void} printOn: oo {ostream reference}
	with: shift {IntegerVar}
	with: numbers {PrimIntegerArray}
	"Print a sequence of numbers separated by dots. Deal with strings specially."
	
	shift < numbers count negated ifTrue:
		[self printArrayOn: oo with: numbers.
		oo << '.'.
		self printZerosOn: oo with: shift negated - numbers count.
		oo << '!!0']
	ifFalse: [shift < IntegerVarZero ifTrue:
		[self printArrayOn: oo with: ((numbers copy: shift negated DOTasLong) cast: PrimIntegerArray).
		oo << '!!'.
		self printArrayOn: oo
			with: ((numbers copy: -1 with: shift negated DOTasLong) cast: PrimIntegerArray)]
	ifFalse:
		[oo << '0!!'.
		shift > IntegerVarZero ifTrue:
			[self printZerosOn: oo with: shift.
			oo << '.'].
		self printArrayOn: oo with: numbers]]!
*/
}
/**
 * Print a sequence of zeros separated by dots. Deal with large numbers specially.
 */
public static void printZerosOn(PrintWriter oo, int shift) {
	if (shift > 7) {
		oo.print("...(");
		oo.print(shift);
		oo.print(")...");
	}
	else {
		for (int i = 0; i < shift - 1; i ++ ) {
			oo.print("0.");
		}
		oo.print("0");
	}
/*
udanax-top.st:32717:Sequence class methodsFor: 'private:'!
{void} printZerosOn: oo {ostream reference}
	with: shift {IntegerVar}
	"Print a sequence of zeros separated by dots. Deal with large numbers specially."
	
	shift > 7 ifTrue:
		[oo << '...(' << shift << ')...']
	ifFalse:
		[IntegerVarZero almostTo: shift - 1 do: [ :i {IntegerVar} |
			oo << '0.'].
		oo << '0']!
*/
}
/**
 * Don't need to make a copy of the array
 */
public static Sequence usingx(int shift, PrimIntegerArray numbers) {
	int start;
	int stop;
	start = numbers.indexPastInteger(0);
	if (start < 0) {
		return zero();
	}
	stop = numbers.indexPastInteger(0, -1, -1);
	if (start != 0 || (stop < (numbers.count() - 1))) {
		return new Sequence(shift + start, ((PrimIntegerArray) (numbers.copy(stop - start, start))));
	}
	else {
		return new Sequence(shift, numbers);
	}
/*
udanax-top.st:32728:Sequence class methodsFor: 'private:'!
{Sequence} usingx: shift {IntegerVar} with: numbers {PrimIntegerArray}
	"Don't need to make a copy of the array"
	
	| start {Int32} stop {Int32} |
	start := numbers indexPastInteger: IntegerVarZero.
	start < Int32Zero ifTrue:
		[^self zero].
	stop := numbers indexPastInteger: IntegerVarZero with: -1 with: -1.
	(start ~= Int32Zero or: [stop < (numbers count - 1)])
		ifTrue: [^self create: shift + start
			with: ((numbers copy: stop - start with: start) cast: PrimIntegerArray)]
		ifFalse: [^self create: shift with: numbers]!
*/
}
public static void initTimeNonInherited() {
	TheZero = new Sequence(0, (IntegerVarArray.zeros(0)));
/*
udanax-top.st:32743:Sequence class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	self REQUIRES: IntegerVarArray.
	TheZero := self create: IntegerVarZero with: (IntegerVarArray zeros: Int32Zero).!
*/
}
public static void linkTimeNonInherited() {
	TheZero = null;
/*
udanax-top.st:32748:Sequence class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheZero := NULL.!
*/
}
/**
 * {IntegerVar CLIENT} firstIndex
 * {IntegerVar CLIENT} integerAt: index {IntegerVar}
 * {PrimIntegerArray CLIENT} integers
 * {BooleanVar CLIENT} isZero
 * {IntegerVar CLIENT} lastIndex
 * {Sequence CLIENT} with: index {IntegerVar} with: number {IntegerVar}
 */
public static void infostProtocol() {
/*
udanax-top.st:32754:Sequence class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IntegerVar CLIENT} firstIndex
{IntegerVar CLIENT} integerAt: index {IntegerVar}
{PrimIntegerArray CLIENT} integers
{BooleanVar CLIENT} isZero
{IntegerVar CLIENT} lastIndex
{Sequence CLIENT} with: index {IntegerVar} with: number {IntegerVar}
"!
*/
}
public Sequence() {
/*

Generated during transformation
*/
}
}
