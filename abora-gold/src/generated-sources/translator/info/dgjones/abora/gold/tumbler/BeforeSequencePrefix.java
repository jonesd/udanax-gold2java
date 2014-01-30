/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.tumbler.BeforeSequencePrefix;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.tumbler.SequenceMapping;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class BeforeSequencePrefix extends SequenceEdge {

	protected int myLimit;
/*
udanax-top.st:63892:
SequenceEdge subclass: #BeforeSequencePrefix
	instanceVariableNames: 'myLimit {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:63896:
(BeforeSequencePrefix getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:64003:
BeforeSequencePrefix class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64006:
(BeforeSequencePrefix getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeforeSequencePrefix.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean follows(Position pos) {
	return ! (sequence().isEqual(((Sequence) pos))) && (sequence().isGE(pos));
/*
udanax-top.st:63901:BeforeSequencePrefix methodsFor: 'comparing'!
{BooleanVar} follows: pos {Position}
	^ (self sequence isEqual: (pos cast: Sequence)) not and: [self sequence isGE: pos]!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) other;
		return myLimit == prefix.limit() && (prefix.sequence().isEqual(sequence()));
	}
	else {
		return false;
	}
/*
udanax-top.st:63905:BeforeSequencePrefix methodsFor: 'comparing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: BeforeSequencePrefix into: [ :prefix |
		^myLimit = prefix limit
			and: [prefix sequence isEqual: self sequence]]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
public boolean isFollowedBy(TransitionEdge next) {
	return false;
/*
udanax-top.st:63914:BeforeSequencePrefix methodsFor: 'comparing'!
{BooleanVar} isFollowedBy: next {TransitionEdge unused}
	^false!
*/
}
public boolean isGE(TransitionEdge other) {
	int diff;
	if (other instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) other;
		diff = sequence().comparePrefix(prefix.sequence(), (Math.min(myLimit, prefix.limit())));
		if (diff != 0) {
			return diff > 0;
		}
		return myLimit >= prefix.limit();
	}
	else if (other instanceof SequenceEdge) {
		SequenceEdge before = (SequenceEdge) other;
		return (sequence().comparePrefix(before.sequence(), myLimit)) > 0;
	}
	return false;
/*
udanax-top.st:63918:BeforeSequencePrefix methodsFor: 'comparing'!
{BooleanVar} isGE: other {TransitionEdge}
	| diff {Int32} |
	other cast: BeforeSequencePrefix into: [ :prefix |
		diff := self sequence comparePrefix: prefix sequence
			with: (myLimit min: prefix limit).
		diff ~= Int32Zero ifTrue:
			[^diff > Int32Zero].
		^myLimit >= prefix limit]
	cast: SequenceEdge into: [ :before |
		^(self sequence comparePrefix: before sequence with: myLimit) > Int32Zero].
	^ false "compiler fodder"!
*/
}
public boolean touches(TransitionEdge other) {
	if (other instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix before = (BeforeSequencePrefix) other;
		return myLimit == before.limit() && ((sequence().comparePrefix(before.sequence(), myLimit - 1)) == 0 && (Math.abs(((sequence().integerAt(myLimit)) - (before.sequence().integerAt(myLimit)))) <= 1));
	}
	else {
		return false;
	}
/*
udanax-top.st:63931:BeforeSequencePrefix methodsFor: 'comparing'!
{BooleanVar} touches: other {TransitionEdge}
	other cast: BeforeSequencePrefix into: [ :before |
		^myLimit = before limit
			and: [(self sequence comparePrefix: before sequence with: myLimit - 1) = Int32Zero
			and: [((self sequence integerAt: myLimit) - (before sequence integerAt: myLimit)) abs <= 1]]]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
public int limit() {
	return myLimit;
/*
udanax-top.st:63943:BeforeSequencePrefix methodsFor: 'accessing'!
{IntegerVar} limit
	^myLimit!
*/
}
public Position position() {
	throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SPACE);
/*
udanax-top.st:63947:BeforeSequencePrefix methodsFor: 'accessing'!
{Position} position
	Heaper BLAST: #NotInSpace.
	^NULL "fodder"!
*/
}
public SequenceEdge transformedBy(SequenceMapping dsp) {
	return new BeforeSequencePrefix(((Sequence) (dsp.of(sequence()))), myLimit + dsp.shift());
/*
udanax-top.st:63952:BeforeSequencePrefix methodsFor: 'accessing'!
{SequenceEdge} transformedBy: dsp {SequenceMapping}
	^BeforeSequencePrefix create: ((dsp of: self sequence) cast: Sequence)
		with: myLimit + dsp shift!
*/
}
public BeforeSequencePrefix(Sequence sequence, int limit) {
	super(sequence);
	myLimit = limit;
/*
udanax-top.st:63959:BeforeSequencePrefix methodsFor: 'create'!
create: sequence {Sequence} with: limit {IntegerVar}
	super create: sequence.
	myLimit := limit.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myLimit);
	oo.print(", ");
	oo.print(sequence());
	oo.print(")");
/*
udanax-top.st:63966:BeforeSequencePrefix methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myLimit << ', ' << self sequence << ')'!
*/
}
public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
	oo.print(" ");
	if (entering) {
		oo.print("(");
	}
	if ( ! (touchesPrevious && ( ! entering))) {
		Ravi.thingToDo();
		for (int i = 
		/* Eliminate strings of zeros / stars, print UInt8Arrays as strings */
		(Math.min(0, sequence().shift())); i <= (Math.max(myLimit + 1, 0)); i ++ ) {
			if (i == 0) {
				oo.print("!");
			}
			else {
				if (i != sequence().shift()) {
					oo.print(".");
				}
			}
			if (i == myLimit && ( ! entering)) {
				oo.print(((sequence().integerAt(i)) - 1));
			}
			else {
				if (i <= myLimit) {
					oo.print((sequence().integerAt(i)));
				}
				else {
					oo.print("*");
				}
			}
		}
	}
	if ( ! (entering)) {
		oo.print(")");
	}
/*
udanax-top.st:63970:BeforeSequencePrefix methodsFor: 'printing'!
{void} printTransitionOn: oo {ostream reference}
	with: entering {BooleanVar}
	with: touchesPrevious {BooleanVar}
	
	oo << ' '.
	entering ifTrue: [oo << '('].
	(touchesPrevious and: [entering not]) ifFalse:
		[Ravi thingToDo. "Eliminate strings of zeros / stars, print UInt8Arrays as strings"
		(IntegerVarZero min: self sequence shift) to: (myLimit + 1 max: IntegerVarZero) do:
			[ :i {IntegerVar} |
			i == IntegerVarZero ifTrue:
				[oo << '!!']
			ifFalse: [i ~= self sequence shift ifTrue:
				[oo << '.']].
			(i = myLimit and: [entering not]) ifTrue:
				[oo << ((self sequence integerAt: i) - 1)]
			ifFalse: [i <= myLimit ifTrue:
				[oo << (self sequence integerAt: i)]
			ifFalse:
				[oo << '*']]]].
	entering ifFalse: [oo << ')']!
*/
}
public BeforeSequencePrefix(Rcvr receiver) {
	super(receiver);
	myLimit = receiver.receiveIntegerVar();
/*
udanax-top.st:63994:BeforeSequencePrefix methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myLimit _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myLimit);
/*
udanax-top.st:63998:BeforeSequencePrefix methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myLimit.!
*/
}
public static TransitionEdge above(Sequence sequence, int limit) {
	if (limit < sequence.shift()) {
		return new BeforeSequencePrefix((Sequence.usingx(limit, ((PrimIntegerArray) (PrimSpec.integerVar().arrayWith((PrimSpec.integerVar().value(1))))))), limit);
	}
	if (limit < (sequence.shift() + sequence.count())) {
		int newCount;
		int hisCount;
		newCount = (limit - sequence.shift() + 1);
		hisCount = sequence.secretNumbers().count();
		return new BeforeSequencePrefix((Sequence.usingx(sequence.shift(), (((PrimIntegerArray) (sequence.secretNumbers().copy((Math.min(newCount, hisCount)), 0, 0, (Math.max(newCount - hisCount, 0))))).hold((limit - sequence.shift()), (sequence.integerAt(limit)) + 1, true)))), limit);
	}
	/* Ravi knownBug. */
	/* creates huge arrays if (limit - sequence shift) is too big */
	return new BeforeSequencePrefix((Sequence.usingx(sequence.shift(), (sequence.secretNumbers().hold((limit - sequence.shift()), (sequence.integerAt(limit)) + 1)))), limit);
/*
udanax-top.st:64011:BeforeSequencePrefix class methodsFor: 'pseudo constructors'!
{TransitionEdge} above: sequence {Sequence} with: limit {IntegerVar}
	limit < sequence shift ifTrue:
		[^self create: (Sequence usingx: limit with: ((PrimSpec integerVar
				arrayWith: (PrimSpec integerVar value: 1)) cast: PrimIntegerArray)) with: limit].
	limit < (sequence shift + sequence count) ifTrue:
		[| newCount {Int32} hisCount {Int32} |
		newCount _ (limit - sequence shift + 1) DOTasLong.
		hisCount _ sequence secretNumbers count.
		^self create: (Sequence
			usingx: sequence shift
			with: (((sequence secretNumbers
							copy: (newCount min: hisCount)
							with: Int32Zero
							with: Int32Zero
							with: (newCount - hisCount max: Int32Zero)) cast: PrimIntegerArray)
				at: (limit - sequence shift) DOTasLong
				hold: (sequence integerAt: limit) + 1
				with: true))
			with: limit].
	"Ravi knownBug." "creates huge arrays if (limit - sequence shift) is too big"
	^self create: (Sequence
			usingx: sequence shift
			with: (sequence secretNumbers
				at: (limit - sequence shift) DOTasLong
				hold: (sequence integerAt: limit) + 1))
		with: limit!
*/
}
public static TransitionEdge below(Sequence sequence, int limit) {
	if (limit < sequence.shift()) {
		return new BeforeSequencePrefix(Sequence.zero(), limit);
	}
	if (limit < (sequence.shift() + sequence.count())) {
		int newCount;
		int hisCount;
		newCount = (limit - sequence.shift() + 1);
		hisCount = sequence.secretNumbers().count();
		return new BeforeSequencePrefix((Sequence.usingx(sequence.shift(), ((PrimIntegerArray) (sequence.secretNumbers().copy((Math.min(newCount, hisCount)), 0, 0, (Math.max(newCount - hisCount, 0))))))), limit);
	}
	return new BeforeSequencePrefix(sequence, limit);
/*
udanax-top.st:64039:BeforeSequencePrefix class methodsFor: 'pseudo constructors'!
{TransitionEdge} below: sequence {Sequence} with: limit {IntegerVar}
	limit < sequence shift ifTrue:
		[^self create: Sequence zero with: limit].
	limit < (sequence shift + sequence count) ifTrue:
		[| newCount {Int32} hisCount {Int32} |
		newCount _ (limit - sequence shift + 1) DOTasLong.
		hisCount _ sequence secretNumbers count.
		^self create: (Sequence
						usingx: sequence shift
						with: ((sequence secretNumbers
							copy: (newCount min: hisCount)
							with: Int32Zero
							with: Int32Zero
							with: (newCount - hisCount max: Int32Zero)) cast: PrimIntegerArray))
				with: limit].
	^self create: sequence with: limit!
*/
}
public BeforeSequencePrefix() {
/*

Generated during transformation
*/
}
}
