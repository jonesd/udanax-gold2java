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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.BasicSpace;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * BasicSpace versus CoordinateSpace is not a type distinction in that there is no difference
 * in contract with the client.  BasicSpace exists as a convenience to the definer of new
 * CoordinateSpaces.  A new subclass of CoordinateSpace should be a subclass of BasicSpace
 * iff there is only one coordinateSpace that corresponds to the new class.  I.e., that the
 * instances are not parameterized to yield different coordinate spaces.  BasicSpace provides
 * some conveniences (especially in Smalltalk) for defining a single canonical instance at
 * dynamic initialization time, and always using it.
 * As this class is irrelevent to CoordinateSpace clients, but is useful to those defining
 * other kinds of coordinate spaces, it is an exellent example of something that would be
 * classified as a "protected" class--something to be persued if we try to make modules more
 * like classes.
 */
public class BasicSpace extends CoordinateSpace {

	protected static BasicSpace theSpace;
/*
udanax-top.st:14503:
CoordinateSpace subclass: #BasicSpace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:14507:
BasicSpace comment:
'BasicSpace versus CoordinateSpace is not a type distinction in that there is no difference in contract with the client.  BasicSpace exists as a convenience to the definer of new CoordinateSpaces.  A new subclass of CoordinateSpace should be a subclass of BasicSpace iff there is only one coordinateSpace that corresponds to the new class.  I.e., that the instances are not parameterized to yield different coordinate spaces.  BasicSpace provides some conveniences (especially in Smalltalk) for defining a single canonical instance at dynamic initialization time, and always using it.
	
	As this class is irrelevent to CoordinateSpace clients, but is useful to those defining other kinds of coordinate spaces, it is an exellent example of something that would be classified as a "protected" class--something to be persued if we try to make modules more like classes.'!
*/
/*
udanax-top.st:14511:
(BasicSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #OBSOLETE; add: #SMALLTALK.ONLY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:14552:
BasicSpace class
	instanceVariableNames: 'theSpace {BasicSpace star} '!
*/
/*
udanax-top.st:14555:
(BasicSpace getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #OBSOLETE; add: #SMALLTALK.ONLY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BasicSpace.class).setAttributes( new Set().add("OBSOLETE").add("SMALLTALKONLY").add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * is equal to any basic space on the same category of positions
 */
public int actualHashForEqual() {
	return getCategory().hashForEqual() + 1;
/*
udanax-top.st:14516:BasicSpace methodsFor: 'testing'!
{UInt32} actualHashForEqual
	"is equal to any basic space on the same category of positions"
	^self getCategory hashForEqual + 1!
*/
}
/**
 * is equal to any basic space on the same category of positions
 */
public boolean isEqual(Heaper anObject) {
	return anObject.getCategory() == getCategory();
/*
udanax-top.st:14521:BasicSpace methodsFor: 'testing'!
{BooleanVar} isEqual: anObject {Heaper}
	"is equal to any basic space on the same category of positions"
	^anObject getCategory == self getCategory!
*/
}
public BasicSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending, OrderSpec descending) {
	super(emptyRegion, fullRegion, identityDsp, ascending, descending);
/*
udanax-top.st:14528:BasicSpace methodsFor: 'creation'!
create: emptyRegion {XnRegion}
	with: fullRegion {XnRegion}
	with: identityDsp {Dsp}
	with: ascending {OrderSpec default: NULL}
	with: descending {OrderSpec default: NULL}
	
	super create: emptyRegion with: fullRegion with: identityDsp with: ascending with: descending.!
*/
}
public BasicSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp) {
	this(emptyRegion, fullRegion, identityDsp, null, null);
/*
udanax-top.st:14538:BasicSpace methodsFor: 'smalltalk: defaults'!
create: emptyRegion {XnRegion}
	with: fullRegion {XnRegion}
	with: identityDsp {Dsp}
	
	self create: emptyRegion with: fullRegion with: identityDsp with: NULL with: NULL!
*/
}
public BasicSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending) {
	this(emptyRegion, fullRegion, identityDsp, ascending, null);
/*
udanax-top.st:14544:BasicSpace methodsFor: 'smalltalk: defaults'!
create: emptyRegion {XnRegion}
	with: fullRegion {XnRegion}
	with: identityDsp {Dsp}
	with: ascending {OrderSpec default: NULL}
	
	self create: emptyRegion with: fullRegion with: identityDsp with: ascending with: NULL!
*/
}
public static void initTimeInherited() {
	theSpace = 
	/* TODO newAllocType */
	new BasicSpace();
/*
udanax-top.st:14560:BasicSpace class methodsFor: 'smalltalk: initialization'!
initTimeInherited
	self REQUIRES: PrimSpec.
	theSpace _ (self new.AllocType: #PERSISTENT) create.!
*/
}
public static void linkTimeInherited() {
	theSpace = null;
/*
udanax-top.st:14565:BasicSpace class methodsFor: 'smalltalk: initialization'!
linkTimeInherited
	theSpace _ NULL.!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:14569:BasicSpace class methodsFor: 'smalltalk: initialization'!
suppressInitTimeInherited!
*/
}
public static void suppressLinkTimeInherited() {
/*
udanax-top.st:14571:BasicSpace class methodsFor: 'smalltalk: initialization'!
suppressLinkTimeInherited!
*/
}
public BasicSpace() {
/*

Generated during transformation
*/
}
public BasicSpace(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
