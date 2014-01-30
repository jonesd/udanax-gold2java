/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.tumbler.AfterSequence;
import info.dgjones.abora.gold.tumbler.BeforeSequencePrefix;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.tumbler.SequenceMapping;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class AfterSequence extends SequenceEdge {

/*
udanax-top.st:63705:
SequenceEdge subclass: #AfterSequence
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:63709:
(AfterSequence getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:63784:
AfterSequence class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63787:
(AfterSequence getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AfterSequence.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Position position() {
	return sequence();
/*
udanax-top.st:63714:AfterSequence methodsFor: 'accessing'!
{Position} position
	^self sequence!
*/
}
public SequenceEdge transformedBy(SequenceMapping dsp) {
	return AfterSequence.make(((Sequence) (dsp.of(sequence()))));
/*
udanax-top.st:63718:AfterSequence methodsFor: 'accessing'!
{SequenceEdge} transformedBy: dsp {SequenceMapping}
	^AfterSequence make: ((dsp of: self sequence) cast: Sequence)!
*/
}
public AfterSequence(Sequence sequence) {
	super(sequence);
/*
udanax-top.st:63724:AfterSequence methodsFor: 'create'!
create: sequence {Sequence}
	super create: sequence.!
*/
}
public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
	oo.print(" ");
	if (entering) {
		oo.print("(");
	}
	if ( ! (touchesPrevious && ( ! entering))) {
		oo.print(sequence());
	}
	if ( ! (entering)) {
		oo.print("]");
	}
/*
udanax-top.st:63730:AfterSequence methodsFor: 'printing'!
{void} printTransitionOn: oo {ostream reference}
	with: entering {BooleanVar}
	with: touchesPrevious {BooleanVar}
	
	oo << ' '.
	entering ifTrue: [oo << '('].
	(touchesPrevious and: [entering not]) ifFalse:
		[oo << self sequence].
	entering ifFalse: [oo << ']']!
*/
}
public boolean follows(Position pos) {
	return sequence().isGE(((Sequence) pos));
/*
udanax-top.st:63743:AfterSequence methodsFor: 'comparing'!
{BooleanVar} follows: pos {Position}
	^self sequence isGE: (pos cast: Sequence)!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof AfterSequence) {
		AfterSequence after = (AfterSequence) other;
		return after.sequence().isEqual(sequence());
	}
	else {
		return false;
	}
/*
udanax-top.st:63747:AfterSequence methodsFor: 'comparing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: AfterSequence into: [ :after |
		^after sequence isEqual: self sequence]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
public boolean isFollowedBy(TransitionEdge next) {
	return false;
/*
udanax-top.st:63755:AfterSequence methodsFor: 'comparing'!
{BooleanVar} isFollowedBy: next {TransitionEdge unused}
	^false!
*/
}
public boolean isGE(TransitionEdge other) {
	if (other instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) other;
		return (sequence().comparePrefix(prefix.sequence(), prefix.limit())) >= 0;
	}
	else if (other instanceof SequenceEdge) {
		SequenceEdge edge = (SequenceEdge) other;
		return sequence().isGE(edge.sequence());
	}
	return false;
/*
udanax-top.st:63759:AfterSequence methodsFor: 'comparing'!
{BooleanVar} isGE: other {TransitionEdge}
	other cast: BeforeSequencePrefix into: [ :prefix |
		^(self sequence comparePrefix: prefix sequence with: prefix limit) >= Int32Zero]
	cast: SequenceEdge into: [ :edge |
		^self sequence isGE: edge sequence].
	^ false "compiler fodder"!
*/
}
public boolean touches(TransitionEdge other) {
	if (other instanceof BeforeSequencePrefix) {
		BeforeSequencePrefix prefix = (BeforeSequencePrefix) other;
		return false;
	}
	else if (other instanceof SequenceEdge) {
		SequenceEdge edge = (SequenceEdge) other;
		return sequence().isEqual(edge.sequence());
	}
	return false;
/*
udanax-top.st:63767:AfterSequence methodsFor: 'comparing'!
{BooleanVar} touches: other {TransitionEdge}
	other cast: BeforeSequencePrefix into: [ :prefix |
		^false]
	cast: SequenceEdge into: [ :edge |
		^self sequence isEqual: edge sequence].
	^ false "compiler fodder"!
*/
}
public AfterSequence(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:63777:AfterSequence methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:63780:AfterSequence methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static SequenceEdge make(Sequence sequence) {
	return new AfterSequence(sequence);
/*
udanax-top.st:63792:AfterSequence class methodsFor: 'pseudo constructors'!
{SequenceEdge} make: sequence {Sequence}
	^self create: sequence!
*/
}
public AfterSequence() {
/*

Generated during transformation
*/
}
}
