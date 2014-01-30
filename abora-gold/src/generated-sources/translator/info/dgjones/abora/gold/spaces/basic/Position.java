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
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This is the superclass of all positions of coordinate spaces.  Each individual position is
 * specific to some one coordinate space.  Positions themselves don''t have much behavior, as
 * most of the interesting aspects of coordinate spaces are defined in the other objects in
 * terms of positions.  Positions do have their own native ordering messages, but for most
 * purposes it''s probably better to compare them using an appropriate OrderSpec.
 */
public class Position extends Heaper {

/*
udanax-top.st:31468:
Heaper subclass: #Position
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:31472:
Position comment:
'This is the superclass of all positions of coordinate spaces.  Each individual position is specific to some one coordinate space.  Positions themselves don''t have much behavior, as most of the interesting aspects of coordinate spaces are defined in the other objects in terms of positions.  Positions do have their own native ordering messages, but for most purposes it''s probably better to compare them using an appropriate OrderSpec.'!
*/
/*
udanax-top.st:31474:
(Position getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:31516:
Position class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:31519:
(Position getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Position.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * since we redefine equal, subclasses had better redefine actualHashForEqual
 */
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:31479:Position methodsFor: 'testing'!
{UInt32} actualHashForEqual
	"since we redefine equal, subclasses had better redefine actualHashForEqual"
	^Heaper takeOop!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31484:Position methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper} 
	self subclassResponsibility!
*/
}
/**
 * Essential.  A region containing this position as its only element.
 */
public XnRegion asRegion() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31490:Position methodsFor: 'accessing'!
{XnRegion CLIENT} asRegion
	"Essential.  A region containing this position as its only element."
	self subclassResponsibility!
*/
}
/**
 * Essential.  The coordinate space this is a position in. This implies that a position
 * object is only a position in one particular coordinate space.
 */
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31494:Position methodsFor: 'accessing'!
{CoordinateSpace CLIENT} coordinateSpace
	"Essential.  The coordinate space this is a position in. This implies that a position object is only a position in one particular coordinate space."
	self subclassResponsibility!
*/
}
/**
 * OBSOLETE. Use OrderSpec instead, or non-polymorphic subclass methods.
 * This must define a full ordering on all positions in the same coordinate space.
 * As this isn''t possible for some coordinate spaces (e.g. HeaperSpace & FilterSpace), we
 * may BLAST instead.  Therefore this message should eventually get retired -- don't use.
 * See OrderSpec::follows for the properties a partial order must satisfy.  A full ordering
 * must additionally satisfy: for all a, b; either a->isAfterOrEqual(b) or
 * b->isAfterOrEqual(a).
 * @deprecated
 */
public boolean isAfterOrEqual(Position other) {
	throw new PasseException();
/*
udanax-top.st:31501:Position methodsFor: 'smalltalk: passe'!
{BooleanVar} isAfterOrEqual: other {Position}
	"OBSOLETE. Use OrderSpec instead, or non-polymorphic subclass methods.
	This must define a full ordering on all positions in the same coordinate space.
	As this isn''t possible for some coordinate spaces (e.g. HeaperSpace & FilterSpace), we may BLAST instead.  Therefore this message should eventually get retired -- don't use.
	See OrderSpec::follows for the properties a partial order must satisfy.  A full ordering must additionally satisfy: for all a, b; either a->isAfterOrEqual(b) or b->isAfterOrEqual(a)."
	self passe!
*/
}
/**
 * OBSOLETE. Use the OrderSpec, or non-polymorphic subclass methods.
 * Defines a transitive partial order; return false if incompatible.  See OrderSpec::follows
 * for the properties a partial order must satisfy.  The ordering according to isGE is the
 * same as the ascending OrderSpec for this coordinate space.  It is probably better to use
 * the OrderSpec than this message.
 * @deprecated
 */
public boolean isGE(Position other) {
	throw new PasseException();
/*
udanax-top.st:31509:Position methodsFor: 'smalltalk: passe'!
{BooleanVar} isGE: other {Position}
	"OBSOLETE. Use the OrderSpec, or non-polymorphic subclass methods.
	Defines a transitive partial order; return false if incompatible.  See OrderSpec::follows for the properties a partial order must satisfy.  The ordering according to isGE is the same as the ascending OrderSpec for this coordinate space.  It is probably better to use the OrderSpec than this message."
	self passe!
*/
}
/**
 * {XuRegion CLIENT} asRegion
 * {CoordinateSpace CLIENT} coordinateSpace
 */
public static void infostProtocol() {
/*
udanax-top.st:31524:Position class methodsFor: 'smalltalk: system'!
info.stProtocol
"{XuRegion CLIENT} asRegion
{CoordinateSpace CLIENT} coordinateSpace
"!
*/
}
public Position() {
/*

Generated during transformation
*/
}
public Position(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
