/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.unordered;

import info.dgjones.abora.gold.hspace.HeaperRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.UnOrdered;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.spaces.unordered.StrongAsPosition;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A position in a HeaperSpace that represents the identity of some particular Heaper.  See
 * class comment in HeaperSpace.
 */
public class HeaperAsPosition extends UnOrdered {

/*
udanax-top.st:32984:
UnOrdered subclass: #HeaperAsPosition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Unordered'!
*/
/*
udanax-top.st:32988:
HeaperAsPosition comment:
'A position in a HeaperSpace that represents the identity of some particular Heaper.  See class comment in HeaperSpace.'!
*/
/*
udanax-top.st:32990:
(HeaperAsPosition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:33023:
HeaperAsPosition class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33026:
(HeaperAsPosition getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HeaperAsPosition.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:32995:HeaperAsPosition methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:32999:HeaperAsPosition methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper} 
	self subclassResponsibility!
*/
}
public XnRegion asRegion() {
	return HeaperRegion.makeHeaperAsPosition(this);
/*
udanax-top.st:33004:HeaperAsPosition methodsFor: 'accessing'!
{XnRegion INLINE} asRegion
	^HeaperRegion make.HeaperAsPosition: self!
*/
}
public CoordinateSpace coordinateSpace() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:33008:HeaperAsPosition methodsFor: 'accessing'!
{CoordinateSpace} coordinateSpace
	self subclassResponsibility!
*/
}
/**
 * Return the underlying Heaper whose identity (as a position) I
 * represent.
 * It is considered good form not to use this message. There is some
 * controversy as to whether it will go away in the future. If you
 * know of any good reason why it should stick around please let us
 * know.
 */
public Heaper heaper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:33011:HeaperAsPosition methodsFor: 'accessing'!
{Heaper} heaper
	"Return the underlying Heaper whose identity (as a position) I 
	represent. 
	
	It is considered good form not to use this message. There is some 
	controversy as to whether it will go away in the future. If you 
	know of any good reason why it should stick around please let us 
	know."
	self subclassResponsibility!
*/
}
/**
 * Return a HeaperAsPosition which represents the identity of this Heaper.  The resulting
 * HeaperAsPosition will strongly retain the original Heaper against garbage collection
 * (though not of course against manual deletion).  See wimpyAsPosition
 */
public static HeaperAsPosition make(Heaper heaper) {
	return new StrongAsPosition(heaper);
/*
udanax-top.st:33031:HeaperAsPosition class methodsFor: 'pseudo constructors'!
{HeaperAsPosition} make: heaper {Heaper}
	"Return a HeaperAsPosition which represents the identity of this Heaper.  The resulting HeaperAsPosition will strongly retain the original Heaper against garbage collection (though not of course against manual deletion).  See wimpyAsPosition"
	
	^StrongAsPosition create: heaper!
*/
}
public HeaperAsPosition() {
/*

Generated during transformation
*/
}
public HeaperAsPosition(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
