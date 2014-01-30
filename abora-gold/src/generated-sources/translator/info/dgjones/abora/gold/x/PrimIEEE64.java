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
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A boxed representation of an IEEE 64-bit floating point value
 */
public class PrimIEEE64 extends PrimFloatValue {

	protected double myValue;
/*
udanax-top.st:34911:
PrimFloatValue subclass: #PrimIEEE64
	instanceVariableNames: 'myValue {IEEE64}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34915:
PrimIEEE64 comment:
'A boxed representation of an IEEE 64-bit floating point value'!
*/
/*
udanax-top.st:34917:
(PrimIEEE64 getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
/*
udanax-top.st:35002:
PrimIEEE64 class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:35005:
(PrimIEEE64 getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIEEE64.class).setAttributes( new Set().add("CONCRETE").add( new String[]
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
udanax-top.st:34922:PrimIEEE64 methodsFor: 'testing'!
{UInt32} actualHashForEqual
	Eric thingToDo. "Figure out a hash for floats"
	[^myValue hash] smalltalkOnly.
	[Eric shouldImplement.
	^ self bitCount] translateOnly.!
*/
}
public boolean isEqual(Heaper other) {
	if ( ! (other instanceof PrimIEEE64)) {
		throw new AboraRuntimeException(AboraRuntimeException.INCOMPARABLE_TYPE);
	}
	return myValue == ((PrimIEEE64) other).asIEEE64();
/*
udanax-top.st:34929:PrimIEEE64 methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	(other isKindOf: PrimIEEE64) ifFalse: [Heaper BLAST: #IncomparableType ].
	^ myValue = (other cast: PrimIEEE64) asIEEE64!
*/
}
public IEEE128 asIEEE128() {
	throw new UnimplementedException();
/*
udanax-top.st:34935:PrimIEEE64 methodsFor: 'smalltalk: accessing'!
{IEEE128} asIEEE128
	
	self unimplemented.
	'IEEE128 a; return a; /- fodder -/' translateOnly!
*/
}
/**
 * The value as an IEEE 32-bit floating point number
 */
public float asIEEE32() {
	return (float) myValue;
/*
udanax-top.st:34942:PrimIEEE64 methodsFor: 'accessing'!
{IEEE32} asIEEE32
	"The value as an IEEE 32-bit floating point number"
	[^ myValue basicCast: IEEE32] translateOnly.
	[^ myValue asFloat] smalltalkOnly.!
*/
}
/**
 * The value as an IEEE 64-bit floating point number
 */
public double asIEEE64() {
	return myValue;
/*
udanax-top.st:34947:PrimIEEE64 methodsFor: 'accessing'!
{IEEE64} asIEEE64
	"The value as an IEEE 64-bit floating point number"
	
	^ myValue!
*/
}
public int bitCount() {
	return 64;
/*
udanax-top.st:34952:PrimIEEE64 methodsFor: 'accessing'!
{Int32} bitCount
	^64!
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
udanax-top.st:34956:PrimIEEE64 methodsFor: 'accessing'!
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
udanax-top.st:34969:PrimIEEE64 methodsFor: 'accessing'!
{BooleanVar} isANumber
	[^ myValue isKindOf: Double] smalltalkOnly.
	'
#if defined(_MSC_VER) || defined(HIGHC) || defined(__sgi) ||  defined (GNUSUN)
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
udanax-top.st:34980:PrimIEEE64 methodsFor: 'accessing'!
{IntegerVar} mantissa
	self unimplemented.
	^IntegerVarZero "fodder"!
*/
}
public PrimIEEE64(double value) {
	super();
	myValue = value;
/*
udanax-top.st:34986:PrimIEEE64 methodsFor: 'protected: create'!
create: value {IEEE64}
	super create.
	myValue := value.!
*/
}
public PrimIEEE64(Rcvr receiver) {
	super(receiver);
	myValue = receiver.receiveIEEEDoubleVar()
	/* TODO was receiveHeaper */
	;
/*
udanax-top.st:34993:PrimIEEE64 methodsFor: 'generated:'!
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
udanax-top.st:34997:PrimIEEE64 methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myValue.!
*/
}
public static PrimIEEE64 make(double value) {
	return new PrimIEEE64(value);
/*
udanax-top.st:35010:PrimIEEE64 class methodsFor: 'create'!
make: value {IEEE64}
	^ self create: value!
*/
}
public PrimIEEE64() {
/*

Generated during transformation
*/
}
}
