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
import info.dgjones.abora.gold.tumbler.AfterReal;
import info.dgjones.abora.gold.tumbler.BeforeReal;
import info.dgjones.abora.gold.tumbler.RealEdge;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class BeforeReal extends RealEdge {

/*
udanax-top.st:63550:
RealEdge subclass: #BeforeReal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:63554:
(BeforeReal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:63615:
BeforeReal class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63618:
(BeforeReal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeforeReal.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
	oo.print(" ");
	if (entering) {
		oo.print("[");
	}
	if ( ! (touchesPrevious && ( ! entering))) {
		oo.print(position());
	}
	if ( ! (entering)) {
		oo.print(")");
	}
/*
udanax-top.st:63559:BeforeReal methodsFor: 'printing'!
{void} printTransitionOn: oo {ostream reference}
	with: entering {BooleanVar}
	with: touchesPrevious {BooleanVar}
	
	oo << ' '.
	entering ifTrue: [oo << '['].
	(touchesPrevious and: [entering not]) ifFalse:
		[oo << self position].
	entering ifFalse: [oo << ')']!
*/
}
public boolean follows(Position pos) {
	return ! (((RealPos) pos).isGE(position()));
/*
udanax-top.st:63571:BeforeReal methodsFor: 'comparing'!
{BooleanVar} follows: pos {Position}
	
	^((pos cast: RealPos) isGE: self position) not!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof BeforeReal) {
		BeforeReal after = (BeforeReal) other;
		return position().isEqual(after.position());
	}
	else {
		return false;
	}
/*
udanax-top.st:63575:BeforeReal methodsFor: 'comparing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: BeforeReal into: [:after |
			^self position isEqual: after position]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFollowedBy(TransitionEdge next) {
	if (next instanceof AfterReal) {
		AfterReal after = (AfterReal) next;
		return position().isEqual(after.position());
	}
	else {
		return false;
	}
/*
udanax-top.st:63583:BeforeReal methodsFor: 'comparing'!
{BooleanVar} isFollowedBy: next {TransitionEdge}
	
	next 
		cast: AfterReal into: [:after |
			^self position isEqual: after position]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isGE(TransitionEdge other) {
	if (other instanceof BeforeReal) {
		BeforeReal before = (BeforeReal) other;
		return position().isGE(before.position());
	}
	else if (other instanceof AfterReal) {
		AfterReal after = (AfterReal) other;
		return ! (after.position().isGE(position()));
	}
	return false;
/*
udanax-top.st:63591:BeforeReal methodsFor: 'comparing'!
{BooleanVar} isGE: other {TransitionEdge}
	
	other 
		cast: BeforeReal into: [:before |
			^self position isGE: before position]
		cast: AfterReal into: [:after |
			^(after position isGE: self position) not].
	^false "fodder"!
*/
}
public BeforeReal(RealPos pos) {
	super(pos);
/*
udanax-top.st:63602:BeforeReal methodsFor: 'creation'!
create: pos {RealPos}
	super create: pos.!
*/
}
public BeforeReal(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:63608:BeforeReal methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:63611:BeforeReal methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static RealEdge make(RealPos pos) {
	return new BeforeReal(pos);
/*
udanax-top.st:63623:BeforeReal class methodsFor: 'create'!
{RealEdge} make: pos {RealPos}
	^self create: pos!
*/
}
public BeforeReal() {
/*

Generated during transformation
*/
}
}
