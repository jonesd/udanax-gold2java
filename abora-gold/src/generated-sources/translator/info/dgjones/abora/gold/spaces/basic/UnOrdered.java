/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.UnOrdered;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A convenient superclass of all Positions which have no natural ordering.  See
 * UnOrdered::isGE for the defining property of this class.  This class should probably go
 * away and UnOrdered::isGE distributed to the subclasses.
 */
public class UnOrdered extends Position {

/*
udanax-top.st:32953:
Position subclass: #UnOrdered
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:32957:
UnOrdered comment:
'A convenient superclass of all Positions which have no natural ordering.  See UnOrdered::isGE for the defining property of this class.  This class should probably go away and UnOrdered::isGE distributed to the subclasses.'!
*/
/*
udanax-top.st:32959:
(UnOrdered getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(UnOrdered.class).setAttributes( new Set().add("NOTATYPE").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public XnRegion asRegion() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:32964:UnOrdered methodsFor: 'accessing'!
{XnRegion} asRegion
	self subclassResponsibility!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:32968:UnOrdered methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:32973:UnOrdered methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Up in position, isGE is deferred, and isEqual is defined in terms of isEqual.
 * Here in UnOrdered, we define isGE in terms of isEqual, so we must redefine
 * isEqual to be deferred.
 */
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:32977:UnOrdered methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper} 
	"Up in position, isGE is deferred, and isEqual is defined in terms of isEqual.
	Here in UnOrdered, we define isGE in terms of isEqual, so we must redefine
	isEqual to be deferred."
	
	self subclassResponsibility!
*/
}
public UnOrdered() {
/*

Generated during transformation
*/
}
public UnOrdered(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
