/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.tumbler.SequenceMapping;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Transforms a Sequence by shifting some amount, and then adding another Sequence to it.
 */
public class SequenceMapping extends Dsp {

	protected int myShift;
	protected Sequence myTranslation;
/*
udanax-top.st:29982:
Dsp subclass: #SequenceMapping
	instanceVariableNames: '
		myShift {IntegerVar}
		myTranslation {Sequence}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:29988:
SequenceMapping comment:
'Transforms a Sequence by shifting some amount, and then adding another Sequence to it.'!
*/
/*
udanax-top.st:29990:
(SequenceMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class SequenceDsp -/
friend class SequenceSpace;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
/*
udanax-top.st:30099:
SequenceMapping class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:30102:
(SequenceMapping getOrMakeCxxClassDescription)
	friends:
'/- friends for class SequenceDsp -/
friend class SequenceSpace;
';
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SequenceMapping.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return SequenceSpace.make();
/*
udanax-top.st:29999:SequenceMapping methodsFor: 'accessing'!
{CoordinateSpace INLINE} coordinateSpace
	^SequenceSpace make!
*/
}
public boolean isIdentity() {
	return myShift == 0 && (myTranslation.isZero());
/*
udanax-top.st:30003:SequenceMapping methodsFor: 'accessing'!
{BooleanVar} isIdentity
	^myShift == IntegerVarZero and: [myTranslation isZero]!
*/
}
/**
 * The amount by which it shifts a sequence
 */
public int shift() {
	return myShift;
/*
udanax-top.st:30007:SequenceMapping methodsFor: 'accessing'!
{IntegerVar CLIENT INLINE} shift
	"The amount by which it shifts a sequence"
	
	^myShift!
*/
}
/**
 * What it adds to a sequence after shifting it
 */
public Sequence translation() {
	return myTranslation;
/*
udanax-top.st:30012:SequenceMapping methodsFor: 'accessing'!
{Sequence CLIENT INLINE} translation
	"What it adds to a sequence after shifting it"
	
	^myTranslation!
*/
}
public Position inverseOf(Position position) {
	if (position instanceof Sequence) {
		Sequence sequence = (Sequence) position;
		return (sequence.minus(myTranslation)).shift( - myShift);
	}
	return null;
/*
udanax-top.st:30019:SequenceMapping methodsFor: 'transforming'!
{Position} inverseOf: position {Position}
	position cast: Sequence into: [ :sequence |
		^(sequence minus: myTranslation) shift: myShift negated].
	^ NULL "compiler fodder"!
*/
}
public XnRegion inverseOfAll(XnRegion reg) {
	Ravi.thingToDo();
	/* make this more efficient */
	return inverse().ofAll(reg);
/*
udanax-top.st:30025:SequenceMapping methodsFor: 'transforming'!
{XnRegion} inverseOfAll: reg {XnRegion}
	Ravi thingToDo. "make this more efficient"
	^self inverse ofAll: reg!
*/
}
public Position of(Position position) {
	if (position instanceof Sequence) {
		Sequence sequence = (Sequence) position;
		return (sequence.shift(myShift)).plus(myTranslation);
	}
	return null;
/*
udanax-top.st:30030:SequenceMapping methodsFor: 'transforming'!
{Position} of: position {Position}
	position cast: Sequence into: [ :sequence |
		^(sequence shift: myShift) plus: myTranslation].
	^ NULL "compiler fodder"!
*/
}
public XnRegion ofAll(XnRegion reg) {
	if (reg instanceof SequenceRegion) {
		SequenceRegion seq = (SequenceRegion) reg;
		PtrArray edges;
		PtrArray newEdges;
		edges = seq.secretTransitions();
		newEdges = PtrArray.nulls(edges.count());
		for (int i = 0; i < edges.count(); i ++ ) {
			newEdges.store(i, (((SequenceEdge) (edges.fetch(i))).transformedBy(this)));
		}
		return SequenceRegion.usingx(seq.startsInside(), newEdges);
	}
	return null;
/*
udanax-top.st:30036:SequenceMapping methodsFor: 'transforming'!
{XnRegion} ofAll: reg {XnRegion}
	reg cast: SequenceRegion into: [ :seq |
		| edges {PtrArray of: SequenceEdge} newEdges {PtrArray of: SequenceEdge} |
		edges := seq secretTransitions.
		newEdges := PtrArray nulls: edges count.
		Int32Zero almostTo: edges count do: [ :i {Int32} |
			newEdges at: i store: (((edges fetch: i) cast: SequenceEdge) transformedBy: self)].
		^SequenceRegion usingx: seq startsInside with: newEdges].
	^NULL "fodder"!
*/
}
/**
 * Return the composition of the two Dsps. Two Dsps of the same space are always composable.
 * (a->compose(b) ->minus(b))->isEqual (a)
 * (a->compose(b) ->of(pos))->isEqual (a->of (b->of (pos))
 */
public Dsp compose(Dsp dsp) {
	if (dsp instanceof SequenceMapping) {
		SequenceMapping other = (SequenceMapping) dsp;
		return SequenceMapping.make(myShift + other.shift(), ((Sequence) (of(other.translation()))));
	}
	return null;
/*
udanax-top.st:30049:SequenceMapping methodsFor: 'combining'!
{Dsp} compose: dsp {Dsp}
	"Return the composition of the two Dsps. Two Dsps of the same space are always composable.
	(a->compose(b) ->minus(b))->isEqual (a)
	(a->compose(b) ->of(pos))->isEqual (a->of (b->of (pos))"
	dsp cast: SequenceMapping into: [ :other {SequenceMapping} |
		^SequenceMapping make: myShift + other shift
			with: ((self of: other translation) cast: Sequence)].
	^ NULL "compiler fodder"!
*/
}
public Mapping inverse() {
	return SequenceMapping.make( - myShift, ((Sequence.zero().minus(myTranslation)).shift(myShift)));
/*
udanax-top.st:30059:SequenceMapping methodsFor: 'combining'!
{Mapping} inverse
	^SequenceMapping make: myShift negated
		with: ((Sequence zero minus: myTranslation) shift: myShift)!
*/
}
public Dsp inverseCompose(Dsp dsp) {
	if (dsp instanceof SequenceMapping) {
		SequenceMapping other = (SequenceMapping) dsp;
		return SequenceMapping.make(myShift - other.shift(), ((Sequence) (inverseOf(other.translation()))));
	}
	return null;
/*
udanax-top.st:30064:SequenceMapping methodsFor: 'combining'!
{Dsp} inverseCompose: dsp {Dsp}
	dsp cast: SequenceMapping into: [ :other |
		^SequenceMapping make: myShift - other shift
			with: ((self inverseOf: other translation) cast: Sequence)].
	^ NULL "compiler fodder"!
*/
}
public Dsp minus(Dsp dsp) {
	if (dsp instanceof SequenceMapping) {
		SequenceMapping other = (SequenceMapping) dsp;
		return SequenceMapping.make(myShift - other.shift(), ((Sequence) (inverseOf(other.translation()))));
	}
	return null;
/*
udanax-top.st:30071:SequenceMapping methodsFor: 'combining'!
{Dsp} minus: dsp {Dsp}
	dsp cast: SequenceMapping into: [ :other |
		^SequenceMapping make: myShift - other shift
			with: ((self inverseOf: other translation) cast: Sequence)].
	^ NULL "compiler fodder"!
*/
}
public SequenceMapping(int shift, Sequence translation) {
	super();
	myShift = shift;
	myTranslation = translation;
/*
udanax-top.st:30080:SequenceMapping methodsFor: 'private: create'!
create: shift {IntegerVar} with: translation {Sequence}
	super create.
	myShift := shift.
	myTranslation := translation.!
*/
}
public SequenceMapping(Rcvr receiver) {
	super(receiver);
	myShift = receiver.receiveIntegerVar();
	myTranslation = (Sequence) receiver.receiveHeaper();
/*
udanax-top.st:30088:SequenceMapping methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myShift _ receiver receiveIntegerVar.
	myTranslation _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myShift);
	xmtr.sendHeaper(myTranslation);
/*
udanax-top.st:30093:SequenceMapping methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myShift.
	xmtr sendHeaper: myTranslation.!
*/
}
public static SequenceMapping make(int shift, Sequence translation) {
	return new SequenceMapping(shift, translation);
/*
udanax-top.st:30111:SequenceMapping class methodsFor: 'private: pseudo constructors'!
make: shift {IntegerVar} with: translation {Sequence}
	^self create: shift with: translation!
*/
}
/**
 * {IntegerVar CLIENT} shift
 * {Sequence CLIENT} translation
 */
public static void infostProtocol() {
/*
udanax-top.st:30117:SequenceMapping class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IntegerVar CLIENT} shift
{Sequence CLIENT} translation
"!
*/
}
public SequenceMapping() {
/*

Generated during transformation
*/
}
}
