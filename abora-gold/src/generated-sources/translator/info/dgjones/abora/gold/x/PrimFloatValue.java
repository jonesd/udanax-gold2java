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
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.IEEE128;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * A boxed representation of a floating point value
 */
public class PrimFloatValue extends PrimValue {

/*
udanax-top.st:34718:
PrimValue subclass: #PrimFloatValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34722:
PrimFloatValue comment:
'A boxed representation of a floating point value'!
*/
/*
udanax-top.st:34724:
(PrimFloatValue getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #(COPY xpp ); yourself)!
*/
/*
udanax-top.st:34789:
PrimFloatValue class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34792:
(PrimFloatValue getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimFloatValue.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add( new String[]
	{"COPY", "xpp"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The value as an IEEE 128-bit floating point number.
 * May not be possible if conversion from subclass to IEEE type is not available.
 */
public IEEE128 asIEEE128() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34729:PrimFloatValue methodsFor: 'smalltalk: accessing'!
{IEEE128} asIEEE128
	"The value as an IEEE 128-bit floating point number.
	May not be possible if conversion from subclass to IEEE type is not available."
	
	self subclassResponsibility!
*/
}
/**
 * The value as an IEEE 32-bit floating point number.
 * May not be possible if conversion from subclass to IEEE type is not available.
 */
public float asIEEE32() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34737:PrimFloatValue methodsFor: 'accessing'!
{IEEE32} asIEEE32
	"The value as an IEEE 32-bit floating point number.
	May not be possible if conversion from subclass to IEEE type is not available."
	
	self subclassResponsibility!
*/
}
/**
 * The value as an IEEE 64-bit floating point number.
 * May not be possible if conversion from subclass to IEEE type is not available.
 */
public double asIEEE64() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34743:PrimFloatValue methodsFor: 'accessing'!
{IEEE64} asIEEE64
	"The value as an IEEE 64-bit floating point number.
	May not be possible if conversion from subclass to IEEE type is not available."
	
	self subclassResponsibility!
*/
}
/**
 * What precision is it, in terms of the number of bits used to represent it.  In the
 * interests of efficiency, this may return a number larger than that *needed* to represent
 * it.  However, the precision reported must be at least that needed to represent this
 * number.  It is assumed that the format of the number satisfies the IEEE radix independent
 * floating point spec.  Should we represent real numbers other that those representable in
 * IEEE, the meaning of this message will be more fully specified.
 * The fact that this message is allowed to overestimate precision doesn't interfere with
 * equality: a->isEqual(b) exactly when they represent that same real number, even if one of
 * them happens to overestimate precision more that the other.
 */
public int bitCount() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34749:PrimFloatValue methodsFor: 'accessing'!
{Int32 CLIENT} bitCount
	"What precision is it, in terms of the number of bits used to represent it.  In the interests of efficiency, this may return a number larger than that *needed* to represent it.  However, the precision reported must be at least that needed to represent this number.  It is assumed that the format of the number satisfies the IEEE radix independent floating point spec.  Should we represent real numbers other that those representable in IEEE, the meaning of this message will be more fully specified.
	The fact that this message is allowed to overestimate precision doesn't interfere with equality: a->isEqual(b) exactly when they represent that same real number, even if one of them happens to overestimate precision more that the other."
	
	self subclassResponsibility!
*/
}
/**
 * If this is a number, return the exponent
 */
public int exponent() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34755:PrimFloatValue methodsFor: 'accessing'!
{IntegerVar} exponent
	"If this is a number, return the exponent"
	
	self subclassResponsibility!
*/
}
/**
 * Return TRUE if value represents a number.
 */
public boolean isANumber() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34760:PrimFloatValue methodsFor: 'accessing'!
{BooleanVar} isANumber
	"Return TRUE if value represents a number."
	
	self subclassResponsibility!
*/
}
/**
 * If this is a number, return the signed mantissa
 */
public int mantissa() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:34765:PrimFloatValue methodsFor: 'accessing'!
{IntegerVar} mantissa
	"If this is a number, return the signed mantissa"
	
	self subclassResponsibility!
*/
}
/**
 * @deprecated
 */
public boolean isNumber() {
	throw new PasseException();
/*
udanax-top.st:34772:PrimFloatValue methodsFor: 'smalltalk: passe'!
{BooleanVar} isNumber
	self passe!
*/
}
/**
 * @deprecated
 */
public int precision() {
	throw new PasseException();
/*
udanax-top.st:34776:PrimFloatValue methodsFor: 'smalltalk: passe'!
{Int32} precision
	self passe "bitCount"!
*/
}
public PrimFloatValue(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:34782:PrimFloatValue methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:34785:PrimFloatValue methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
/**
 * {IEEE128 CLIENT} asIEEE128
 * {IEEE128 CLIENT} asIEEE128Approximation
 * {IEEE32 CLIENT} asIEEE32
 * {IEEE32 CLIENT} asIEEE32Approximation
 * {IEEE64 CLIENT} asIEEE64
 * {IEEE64 CLIENT} asIEEE64Approximation
 * {IntegerVar CLIENT} exponent
 * {BooleanVar CLIENT} isANumber
 * {IntegerVar CLIENT} mantissa
 * {Int32 CLIENT} precision
 */
public static void infostProtocol() {
/*
udanax-top.st:34797:PrimFloatValue class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IEEE128 CLIENT} asIEEE128
{IEEE128 CLIENT} asIEEE128Approximation
{IEEE32 CLIENT} asIEEE32
{IEEE32 CLIENT} asIEEE32Approximation
{IEEE64 CLIENT} asIEEE64
{IEEE64 CLIENT} asIEEE64Approximation
{IntegerVar CLIENT} exponent
{BooleanVar CLIENT} isANumber
{IntegerVar CLIENT} mantissa
{Int32 CLIENT} precision
"!
*/
}
public PrimFloatValue() {
/*

Generated during transformation
*/
}
}
