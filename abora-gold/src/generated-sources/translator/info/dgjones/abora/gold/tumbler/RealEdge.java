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
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.tumbler.RealEdge;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class RealEdge extends TransitionEdge {

	protected RealPos myPos;
/*
udanax-top.st:63418:
TransitionEdge subclass: #RealEdge
	instanceVariableNames: 'myPos {RealPos}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:63422:
(RealEdge getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RealEdge.class).setAttributes( new Set().add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public RealPos position() {
	return myPos;
/*
udanax-top.st:63427:RealEdge methodsFor: 'accessing'!
{RealPos} position
	^myPos!
*/
}
public int actualHashForEqual() {
	return myPos.hashForEqual() ^ getCategory().hashForEqual();
/*
udanax-top.st:63433:RealEdge methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^myPos hashForEqual bitXor: self getCategory hashForEqual!
*/
}
public boolean follows(Position pos) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63437:RealEdge methodsFor: 'testing'!
{BooleanVar} follows: pos {Position}
	
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63441:RealEdge methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	self subclassResponsibility!
*/
}
public boolean isFollowedBy(TransitionEdge next) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63445:RealEdge methodsFor: 'testing'!
{BooleanVar} isFollowedBy: next {TransitionEdge}
	
	self subclassResponsibility!
*/
}
public boolean isGE(TransitionEdge other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63449:RealEdge methodsFor: 'testing'!
{BooleanVar} isGE: other {TransitionEdge}
	
	self subclassResponsibility!
*/
}
public boolean touches(TransitionEdge other) {
	return myPos.isEqual(((RealEdge) other).position());
/*
udanax-top.st:63453:RealEdge methodsFor: 'testing'!
{BooleanVar} touches: other {TransitionEdge}
	
	^myPos isEqual: (other cast: RealEdge) position!
*/
}
public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63459:RealEdge methodsFor: 'printing'!
{void} printTransitionOn: oo {ostream reference}
	with: entering {BooleanVar}
	with: touchesPrevious {BooleanVar}
	
	self subclassResponsibility!
*/
}
public RealEdge(RealPos pos) {
	super();
	myPos = pos;
/*
udanax-top.st:63467:RealEdge methodsFor: 'creation'!
create: pos {RealPos}
	super create.
	myPos := pos.!
*/
}
public RealEdge(Rcvr receiver) {
	super(receiver);
	myPos = (RealPos) receiver.receiveHeaper();
/*
udanax-top.st:63474:RealEdge methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPos _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myPos);
/*
udanax-top.st:63478:RealEdge methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myPos.!
*/
}
public RealEdge() {
/*

Generated during transformation
*/
}
}
