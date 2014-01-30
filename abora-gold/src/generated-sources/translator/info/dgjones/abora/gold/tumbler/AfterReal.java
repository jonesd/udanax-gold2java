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
import info.dgjones.abora.gold.tumbler.RealEdge;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class AfterReal extends RealEdge {

/*
udanax-top.st:63482:
RealEdge subclass: #AfterReal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:63486:
(AfterReal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:63538:
AfterReal class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63541:
(AfterReal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AfterReal.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean follows(Position pos) {
	return position().isGE(((RealPos) pos));
/*
udanax-top.st:63491:AfterReal methodsFor: 'comparing'!
{BooleanVar} follows: pos {Position}
	
	^self position isGE: (pos cast: RealPos)!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof AfterReal) {
		AfterReal after = (AfterReal) other;
		return position().isEqual(after.position());
	}
	else {
		return false;
	}
/*
udanax-top.st:63495:AfterReal methodsFor: 'comparing'!
{BooleanVar} isEqual: other {Heaper}
	other
		cast: AfterReal into: [:after |
			^self position isEqual: after position]
		others: [^false].
	^false "fodder"!
*/
}
public boolean isFollowedBy(TransitionEdge next) {
	return false;
/*
udanax-top.st:63503:AfterReal methodsFor: 'comparing'!
{BooleanVar} isFollowedBy: next {TransitionEdge}
	^false!
*/
}
public boolean isGE(TransitionEdge other) {
	return position().isGE(((RealEdge) other).position());
/*
udanax-top.st:63507:AfterReal methodsFor: 'comparing'!
{BooleanVar} isGE: other {TransitionEdge}
	
	^self position isGE: (other cast: RealEdge) position!
*/
}
public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
	oo.print(" ");
	if (entering) {
		oo.print("(");
	}
	if ( ! (touchesPrevious && ( ! entering))) {
		oo.print(position());
	}
	if ( ! (entering)) {
		oo.print("]");
	}
/*
udanax-top.st:63513:AfterReal methodsFor: 'printing'!
{void} printTransitionOn: oo {ostream reference}
	with: entering {BooleanVar}
	with: touchesPrevious {BooleanVar}
	
	oo << ' '.
	entering ifTrue: [oo << '('].
	(touchesPrevious and: [entering not]) ifFalse:
		[oo << self position].
	entering ifFalse: [oo << ']']!
*/
}
public AfterReal(RealPos pos) {
	super(pos);
/*
udanax-top.st:63525:AfterReal methodsFor: 'creation'!
create: pos {RealPos}
	super create: pos.!
*/
}
public AfterReal(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:63531:AfterReal methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:63534:AfterReal methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static RealEdge make(RealPos pos) {
	return new AfterReal(pos);
/*
udanax-top.st:63546:AfterReal class methodsFor: 'create'!
{RealEdge} make: pos {RealPos}
	^self create: pos!
*/
}
public AfterReal() {
/*

Generated during transformation
*/
}
}
