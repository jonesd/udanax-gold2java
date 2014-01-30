/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.SequenceTester;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class SequenceTester extends RegionTester {

/*
udanax-top.st:60138:
RegionTester subclass: #SequenceTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:60142:
(SequenceTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
/*
udanax-top.st:60212:
SequenceTester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:60215:
(SequenceTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SequenceTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public ImmuSet initExamples() {
	SetAccumulator sequences;
	SetAccumulator result;
	ImmuSet base;
	sequences = SetAccumulator.make();
	sequences.step((Sequence.zero()));
	sequences.step((Sequence.one(1)));
	sequences.step((Sequence.two(1, 2)));
	sequences.step((Sequence.two(1, -2)));
	Stepper stomper = ((ImmuSet) sequences.value()).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Sequence tum = (Sequence) stomper.fetch();
		if (tum == null) {
			continue ;
		}
		sequences.step((tum.shift(2)));
		sequences.step((tum.shift(-2)));
	}
	stomper.destroy();
	result = SetAccumulator.make();
	Stepper stomper2 = ((ScruSet) sequences.value()).stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		Sequence sequence = (Sequence) stomper2.fetch();
		if (sequence == null) {
			continue ;
		}
		result.step((SequenceSpace.make().prefixedBy(sequence, sequence.shift() + sequence.count())));
		result.step((SequenceSpace.make().prefixedBy(sequence, sequence.shift() + sequence.count())).complement());
		result.step((SequenceSpace.make().above(sequence, true)));
		result.step((SequenceSpace.make().above(sequence, false)));
		result.step((SequenceSpace.make().below(sequence, true)));
		result.step((SequenceSpace.make().below(sequence, false)));
	}
	stomper2.destroy();
	base = (ImmuSet) result.value();
	Stepper stomper3 = base.stepper();
	for (; stomper3.hasValue(); stomper3.step()) {
		SequenceRegion r = (SequenceRegion) stomper3.fetch();
		if (r == null) {
			continue ;
		}
		Stepper stomper4 = base.stepper();
		for (; stomper4.hasValue(); stomper4.step()) {
			SequenceRegion r2 = (SequenceRegion) stomper4.fetch();
			if (r2 == null) {
				continue ;
			}
			if (r.hashForEqual() < r2.hashForEqual()) {
				result.step((r.unionWith(r2)));
				result.step((r.intersect(r2)));
			}
		}
		stomper4.destroy();
	}
	stomper3.destroy();
	return (ImmuSet) result.value();
/*
udanax-top.st:60147:SequenceTester methodsFor: 'init'!
{ImmuSet of: XnRegion} initExamples
	| sequences {SetAccumulator} result {SetAccumulator} base {ImmuSet} |
	sequences := SetAccumulator make.
	sequences step: (Sequence zero).
	sequences step: (Sequence one: 1).
	sequences step: (Sequence two: 1 with: 2).
	sequences step: (Sequence two: 1 with: -2).
	(sequences value cast: ImmuSet) stepper forEach: [ :tum {Sequence} |
		sequences step: (tum shift: 2).
		sequences step: (tum shift: -2)].
	result := SetAccumulator make.
	(sequences value cast: ScruSet) stepper forEach: [ :sequence {Sequence} |
		result step: (SequenceSpace make prefixedBy: sequence
			with: sequence shift + sequence count).
		result step: (SequenceSpace make prefixedBy: sequence
			with: sequence shift + sequence count) complement.
		result step: (SequenceSpace make above: sequence with: true).
		result step: (SequenceSpace make above: sequence with: false).
		result step: (SequenceSpace make below: sequence with: true).
		result step: (SequenceSpace make below: sequence with: false)].
	base := result value cast: ImmuSet.
	base stepper forEach: [ :r {SequenceRegion} |
		base stepper forEach: [ :r2 {SequenceRegion} |
			r hashForEqual < r2 hashForEqual ifTrue:
				[result step: (r unionWith: r2).
				result step: (r intersect: r2)]]].
	^result value cast: ImmuSet!
*/
}
public void testExtraOn(PrintWriter oo) {
	Sequence withLeadingZeros;
	Sequence withoutLeadingZeros;
	Sequence withTrailingZeros;
	Sequence withoutTrailingZeros;
	withLeadingZeros = Sequence.numbers(((PrimIntegerArray) (PrimSpec.integerVar().arrayWithThree((PrimSpec.integerVar().value(0)), (PrimSpec.integerVar().value(2)), (PrimSpec.integerVar().value(5))))));
	withoutLeadingZeros = (Sequence.numbers(((PrimIntegerArray) (PrimSpec.integerVar().arrayWithTwo((PrimSpec.integerVar().value(2)), (PrimSpec.integerVar().value(5))))))).shift(1);
	if ( ! (withLeadingZeros.hashForEqual() == withoutLeadingZeros.hashForEqual())) {
		oo.print("Sequence::numbers() misses leading zeros");
	}
	else {
		oo.print("Sequence::numbers() correctly counts leading zeros");
	}
	oo.print("\n"+
"");
	withTrailingZeros = Sequence.numbers(((PrimIntegerArray) (PrimSpec.integerVar().arrayWithThree((PrimSpec.integerVar().value(5)), (PrimSpec.integerVar().value(2)), (PrimSpec.integerVar().value(0))))));
	withoutTrailingZeros = ( new Sequence(0, ((PrimIntegerArray) (PrimSpec.integerVar().arrayWithTwo((PrimSpec.integerVar().value(5)), (PrimSpec.integerVar().value(2)))))));
	if ( ! (withTrailingZeros.hashForEqual() == withoutTrailingZeros.hashForEqual())) {
		oo.print("Sequence::numbers() misses trailing zeros");
	}
	else {
		oo.print("Sequence::numbers() correctly counts trailing zeros");
	}
	oo.print("\n"+
"");
/*
udanax-top.st:60177:SequenceTester methodsFor: 'testing'!
{void} testExtraOn: oo {ostream reference}
	| withLeadingZeros {Sequence} withoutLeadingZeros {Sequence} withTrailingZeros {Sequence} withoutTrailingZeros {Sequence} |
	withLeadingZeros _ Sequence numbers: ((PrimSpec integerVar arrayWithThree: (PrimSpec integerVar value: IntegerVarZero)
		  										 with: (PrimSpec integerVar value: 2) 
		  										 with: (PrimSpec integerVar value: 5)) cast: PrimIntegerArray).  
	withoutLeadingZeros _ (Sequence numbers: ((PrimSpec integerVar arrayWithTwo: (PrimSpec integerVar value: 2) 
		  										 with: (PrimSpec integerVar value: 5)) cast: PrimIntegerArray)) shift: 1.
	(withLeadingZeros hashForEqual == withoutLeadingZeros hashForEqual)
		ifFalse: [oo << 'Sequence::numbers() misses leading zeros']
		ifTrue: [oo << 'Sequence::numbers() correctly counts leading zeros'].
	oo << '
'.
	withTrailingZeros _ Sequence numbers: ((PrimSpec integerVar arrayWithThree: (PrimSpec integerVar value: 5)
		  										 with: (PrimSpec integerVar value: 2) 
		  										 with: (PrimSpec integerVar value: IntegerVarZero)) cast: PrimIntegerArray).  
	withoutTrailingZeros _ (Sequence create: IntegerVarZero
		    with:  ((PrimSpec integerVar arrayWithTwo: (PrimSpec integerVar value: 5) 
		  										 with: (PrimSpec integerVar value: 2)) cast: PrimIntegerArray)).
	(withTrailingZeros hashForEqual == withoutTrailingZeros hashForEqual)
		ifFalse: [oo << 'Sequence::numbers() misses trailing zeros'.]
		ifTrue: [oo << 'Sequence::numbers() correctly counts trailing zeros'.].
	oo << '
'.!
*/
}
public SequenceTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60205:SequenceTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60208:SequenceTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public SequenceTester() {
/*

Generated during transformation
*/
}
}
