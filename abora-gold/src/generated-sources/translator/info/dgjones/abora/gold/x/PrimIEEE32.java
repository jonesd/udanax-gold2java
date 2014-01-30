/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.x;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.HashHelper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.IEEE128;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A boxed representation of an IEEE 32-bit floating point value
 */
public class PrimIEEE32 extends PrimFloatValue {

	protected float myValue;
/*
udanax-top.st:34810:
PrimFloatValue subclass: #PrimIEEE32
	instanceVariableNames: 'myValue {IEEE32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34814:
PrimIEEE32 comment:
'A boxed representation of an IEEE 32-bit floating point value'!
*/
/*
udanax-top.st:34816:
(PrimIEEE32 getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
/*
udanax-top.st:34900:
PrimIEEE32 class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34903:
(PrimIEEE32 getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIEEE32.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "xpp"}));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	Eric.thingToDo();
	/* Figure out a hash for floats */
	return HashHelper.hashForEqual(myValue);
/*
udanax-top.st:34821:PrimIEEE32 methodsFor: 'testing'!
{UInt32} actualHashForEqual
	Eric thingToDo. "Figure out a hash for floats"
	[^myValue hash] smalltalkOnly.
	[Eric shouldImplement.
	^ self bitCount] translateOnly.!
*/
}
public boolean isEqual(Heaper other) {
	if ( ! (other instanceof PrimIEEE32)) {
		throw new AboraRuntimeException(AboraRuntimeException.INCOMPARABLE_TYPE);
	}
	return myValue == ((PrimIEEE32) other).asIEEE32();
/*
udanax-top.st:34828:PrimIEEE32 methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	(other isKindOf: PrimIEEE32) ifFalse: [Heaper BLAST: #IncomparableType ].
	^ myValue = (other cast: PrimIEEE32) asIEEE32!
*/
}
public IEEE128 asIEEE128() {
	throw new UnimplementedException();
/*
udanax-top.st:34834:PrimIEEE32 methodsFor: 'smalltalk: accessing'!
{IEEE128} asIEEE128
	
	self unimplemented.
	'IEEE128 a; return a; /- fodder -/' translateOnly!
*/
}
/**
 * The value as an IEEE 32-bit floating point number
 */
public float asIEEE32() {
	return myValue;
/*
udanax-top.st:34841:PrimIEEE32 methodsFor: 'accessing'!
{IEEE32} asIEEE32
	"The value as an IEEE 32-bit floating point number"
	^ myValue!
*/
}
/**
 * The value as an IEEE 64-bit floating point number
 */
public double asIEEE64() {
	return (double) myValue;
/*
udanax-top.st:34845:PrimIEEE32 methodsFor: 'accessing'!
{IEEE64} asIEEE64
	"The value as an IEEE 64-bit floating point number"
	[^ myValue basicCast: IEEE64] translateOnly.
	[^ myValue asDouble] smalltalkOnly.!
*/
}
public int bitCount() {
	return 32;
/*
udanax-top.st:34850:PrimIEEE32 methodsFor: 'accessing'!
{Int32} bitCount
	^32!
*/
}
/**
 * If this is a number, return the exponent
 */
public int exponent() {
	if ( ! (isANumber())) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ANUMBER);
	}
	return AboraSupport.exponent(myValue);
/*
udanax-top.st:34854:PrimIEEE32 methodsFor: 'accessing'!
{IntegerVar} exponent
	"If this is a number, return the exponent"
	self isANumber ifFalse: [Heaper BLAST: #NotANumber].
	[^ myValue exponent] smalltalkOnly.
	'
#if defined(_MSC_VER) || defined(HIGHC) || defined(__sgi) ||  defined (GNUSUN)
	BLAST(NOT_YET_IMPLEMENTED);
	return IntegerVarZero; /- fodder -/
#else
	return myValue == 0 ? 0 : ilogb(myValue);
#endif /- WIN32 -/
' translateOnly.!
*/
}
public boolean isANumber() {
	/* Transform: Rewrote body */
	return AboraSupport.isANumber(myValue);
/*
udanax-top.st:34867:PrimIEEE32 methodsFor: 'accessing'!
{BooleanVar} isANumber
	[^ myValue isKindOf: Float] smalltalkOnly.
	'
#if defined(_MSC_VER) || defined(HIGHC) || defined(__sgi)||  defined (GNUSUN)
	BLAST(NOT_YET_IMPLEMENTED);
	return FALSE; /- fodder -/
#else
	return finite(myValue);
#endif /- WIN32 -/
' translateOnly.!
*/
}
public int mantissa() {
	throw new UnimplementedException();
/*
udanax-top.st:34878:PrimIEEE32 methodsFor: 'accessing'!
{IntegerVar} mantissa
	self unimplemented.
	^IntegerVarZero "fodder"!
*/
}
public PrimIEEE32(float value) {
	super();
	myValue = value;
/*
udanax-top.st:34884:PrimIEEE32 methodsFor: 'protected: create'!
create: value {IEEE32}
	super create.
	myValue := value.!
*/
}
public PrimIEEE32(Rcvr receiver) {
	super(receiver);
	myValue = (float) receiver.receiveIEEEDoubleVar()
	/* TODO was receiveHeaper */
	;
/*
udanax-top.st:34891:PrimIEEE32 methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myValue _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIEEEDoubleVar(myValue)
	/* TODO was sendHeaper */
	;
/*
udanax-top.st:34895:PrimIEEE32 methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myValue.!
*/
}
public static PrimIEEE32 make(float value) {
	return new PrimIEEE32(value);
/*
udanax-top.st:34908:PrimIEEE32 class methodsFor: 'create'!
make: value {IEEE32}
	^ self create: value!
*/
}
public PrimIEEE32() {
/*

Generated during transformation
*/
}
}
