/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.edgeregion;

import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * Clients of EdgeManager define concrete subclasses of this, which are then used by the
 * EdgeManager code
 */
public class TransitionEdge extends Heaper {

/*
udanax-top.st:63348:
Heaper subclass: #TransitionEdge
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-EdgeRegion'!
*/
/*
udanax-top.st:63352:
TransitionEdge comment:
'Clients of EdgeManager define concrete subclasses of this, which are then used by the EdgeManager code'!
*/
/*
udanax-top.st:63354:
(TransitionEdge getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TransitionEdge.class).setAttributes( new Set().add("DEFERRED").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public TransitionEdge ceiling(TransitionEdge other) {
	if (other.isGE(this)) {
		return other;
	}
	else {
		return this;
	}
/*
udanax-top.st:63359:TransitionEdge methodsFor: 'accessing'!
{TransitionEdge} ceiling: other {TransitionEdge}
	(other isGE: self)
		ifTrue: [^other]
		ifFalse: [^self]!
*/
}
public TransitionEdge floor(TransitionEdge other) {
	if (isGE(other)) {
		return other;
	}
	else {
		return this;
	}
/*
udanax-top.st:63365:TransitionEdge methodsFor: 'accessing'!
{TransitionEdge} floor: other {TransitionEdge}
	(self isGE: other)
		ifTrue: [^other]
		ifFalse: [^self]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:63373:TransitionEdge methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Whether the position is strictly less than this edge
 */
public boolean follows(Position pos) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63377:TransitionEdge methodsFor: 'testing'!
{BooleanVar} follows: pos {Position}
	"Whether the position is strictly less than this edge"
	
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63382:TransitionEdge methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	self subclassResponsibility!
*/
}
/**
 * Whether there is precisely one position between this edge and the next one
 */
public boolean isFollowedBy(TransitionEdge next) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63386:TransitionEdge methodsFor: 'testing'!
{BooleanVar} isFollowedBy: next {TransitionEdge}
	"Whether there is precisely one position between this edge and the next one"
	
	self subclassResponsibility!
*/
}
/**
 * Defines a full ordering among all edges in a given CoordinateSpace
 */
public boolean isGE(TransitionEdge other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63391:TransitionEdge methodsFor: 'testing'!
{BooleanVar} isGE: other {TransitionEdge}
	"Defines a full ordering among all edges in a given CoordinateSpace"
	
	self subclassResponsibility!
*/
}
/**
 * Whether this edge touches the same position the other does
 */
public boolean touches(TransitionEdge other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63396:TransitionEdge methodsFor: 'testing'!
{BooleanVar} touches: other {TransitionEdge}
	"Whether this edge touches the same position the other does"
	
	self subclassResponsibility!
*/
}
/**
 * Print a description of this transition
 */
public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:63403:TransitionEdge methodsFor: 'printing'!
{void} printTransitionOn: oo {ostream reference}
	with: entering {BooleanVar}
	with: touchesPrevious {BooleanVar}
	"Print a description of this transition"
	
	self subclassResponsibility!
*/
}
public TransitionEdge(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:63412:TransitionEdge methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:63415:TransitionEdge methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public TransitionEdge() {
/*

Generated during transformation
*/
}
}
