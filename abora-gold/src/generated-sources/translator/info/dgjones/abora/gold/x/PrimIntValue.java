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
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class PrimIntValue extends PrimValue {

	protected int myValue;
/*
udanax-top.st:35013:
PrimValue subclass: #PrimIntValue
	instanceVariableNames: 'myValue {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:35017:
(PrimIntValue getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #(COPY xpp ); yourself)!
*/
/*
udanax-top.st:35158:
PrimIntValue class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:35161:
(PrimIntValue getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIntValue.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add( new String[]
	{"COPY", "xpp"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return the the first number bitwise and'd with the second.
 */
public int bitwiseAnd(PrimIntValue another) {
	return myValue & another.asIntegerVar();
/*
udanax-top.st:35022:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} bitwiseAnd: another {PrimIntValue}
	"Return the the first number bitwise and'd with the second."
	
	^myValue bitAnd: another asIntegerVar!
*/
}
/**
 * Return the the first number bitwise or'd with the second.
 */
public int bitwiseOr(PrimIntValue another) {
	return myValue | another.asIntegerVar();
/*
udanax-top.st:35027:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} bitwiseOr: another {PrimIntValue}
	"Return the the first number bitwise or'd with the second."
	
	^myValue bitOr: another asIntegerVar!
*/
}
/**
 * Return the the first number bitwise xor'd with the second.
 */
public int bitwiseXor(PrimIntValue another) {
	return myValue ^ another.asIntegerVar();
/*
udanax-top.st:35032:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} bitwiseXor: another {PrimIntValue}
	"Return the the first number bitwise xor'd with the second."
	
	^myValue bitXor: another asIntegerVar!
*/
}
/**
 * Integer divide the two numbers and return the result.  This truncates.
 */
public int dividedBy(PrimIntValue another) {
	return myValue / another.asIntegerVar();
/*
udanax-top.st:35037:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} dividedBy: another {PrimIntValue}
	"Integer divide the two numbers and return the result.  This truncates."
	
	^myValue // another asIntegerVar!
*/
}
/**
 * Return true if the first number is greater than or euqla to the second number.
 */
public boolean isGE(PrimIntValue another) {
	return myValue >= another.asIntegerVar();
/*
udanax-top.st:35042:PrimIntValue methodsFor: 'operations'!
{BooleanVar CLIENT login} isGE: another {PrimIntValue}
	"Return true if the first number is greater than or euqla to the second number."
	
	^myValue >= another asIntegerVar!
*/
}
/**
 * Return the the first number shifted to the left by the second amount.
 */
public int leftShift(PrimIntValue another) {
	return myValue << another.asIntegerVar();
/*
udanax-top.st:35047:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} leftShift: another {PrimIntValue}
	"Return the the first number shifted to the left by the second amount."
	
	^myValue bitShift: another asIntegerVar!
*/
}
/**
 * Return the largest of the two numbers.
 */
public int maximum(PrimIntValue another) {
	return Math.max(myValue, another.asIntegerVar());
/*
udanax-top.st:35052:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} maximum: another {PrimIntValue}
	"Return the largest of the two numbers."
	
	^myValue max: another asIntegerVar!
*/
}
/**
 * Return the smallest of the two numbers.
 */
public int minimum(PrimIntValue another) {
	return Math.min(myValue, another.asIntegerVar());
/*
udanax-top.st:35057:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} minimum: another {PrimIntValue}
	"Return the smallest of the two numbers."
	
	^myValue min: another asIntegerVar!
*/
}
/**
 * Return the difference two numbers.
 */
public int minus(PrimIntValue another) {
	return myValue - another.asIntegerVar();
/*
udanax-top.st:35062:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} minus: another {PrimIntValue}
	"Return the difference two numbers."
	
	^myValue - another asIntegerVar!
*/
}
/**
 * Return the the first number modulo the second.
 */
public int mod(PrimIntValue another) {
	return AboraSupport.modulo(myValue, another.asIntegerVar());
/*
udanax-top.st:35067:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} mod: another {PrimIntValue}
	"Return the the first number modulo the second."
	
	^myValue \\ another asIntegerVar!
*/
}
/**
 * Return the sum of two numbers.
 */
public int plus(PrimIntValue another) {
	return myValue + another.asIntegerVar();
/*
udanax-top.st:35072:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} plus: another {PrimIntValue}
	"Return the sum of two numbers."
	
	^myValue + another asIntegerVar!
*/
}
/**
 * Multiply the two numbers and return the result.
 */
public int times(PrimIntValue another) {
	return myValue * another.asIntegerVar();
/*
udanax-top.st:35077:PrimIntValue methodsFor: 'operations'!
{IntegerVar CLIENT login} times: another {PrimIntValue}
	"Multiply the two numbers and return the result."
	
	^myValue * another asIntegerVar!
*/
}
/**
 * The value as a BooleanVar.
 */
public boolean asBooleanVar() {
	return myValue != 0;
/*
udanax-top.st:35084:PrimIntValue methodsFor: 'accessing'!
{BooleanVar INLINE} asBooleanVar
	"The value as a BooleanVar."
	
	^myValue ~~ IntegerVarZero!
*/
}
/**
 * The value as a 32 bit signed integer
 */
public int asInt32() {
	return myValue;
/*
udanax-top.st:35089:PrimIntValue methodsFor: 'accessing'!
{Int32 INLINE} asInt32
	"The value as a 32 bit signed integer"
	
	^myValue DOTasInt32!
*/
}
/**
 * The value as an indefinite precision integer
 */
public int asIntegerVar() {
	return myValue;
/*
udanax-top.st:35094:PrimIntValue methodsFor: 'accessing'!
{IntegerVar INLINE} asIntegerVar
	"The value as an indefinite precision integer"
	
	^myValue!
*/
}
/**
 * The value as a 32 bit unsigned integer
 */
public int asUInt32() {
	return myValue;
/*
udanax-top.st:35099:PrimIntValue methodsFor: 'accessing'!
{UInt32 INLINE} asUInt32
	"The value as a 32 bit unsigned integer"
	
	^myValue DOTasUInt32!
*/
}
/**
 * The value as a 8 bit unsigned integer
 */
public int asUInt8() {
	return myValue;
/*
udanax-top.st:35104:PrimIntValue methodsFor: 'accessing'!
{UInt8 INLINE} asUInt8
	"The value as a 8 bit unsigned integer"
	
	^myValue DOTasUInt32!
*/
}
/**
 * What precision is it, in terms of the number of bits used to represent it.  In the
 * interests of efficiency, this may return a number larger than that *needed* to represent
 * it.  However, the precision reported must be at least that needed to represent this
 * number.
 * The fact that this message is allowed to overestimate precision doesn't interfere with
 * equality: a->isEqual(b) exactly when they represent that same real number, even if one of
 * them happens to overestimate precision more that the other.
 */
public int bitCount() {
	int precision;
	precision = (PrimSpec.toHold(myValue)).bitCount();
	if (precision == 0) {
		throw new AboraRuntimeException(AboraRuntimeException.NO_BIT_COUNT_LIMIT);
	}
	return precision;
/*
udanax-top.st:35109:PrimIntValue methodsFor: 'accessing'!
{Int32 CLIENT} bitCount
	"What precision is it, in terms of the number of bits used to represent it.  In the interests of efficiency, this may return a number larger than that *needed* to represent it.  However, the precision reported must be at least that needed to represent this number.
	The fact that this message is allowed to overestimate precision doesn't interfere with equality: a->isEqual(b) exactly when they represent that same real number, even if one of them happens to overestimate precision more that the other."
	
	| precision {Int32} |
	precision _ (PrimSpec toHold: myValue) bitCount.
	precision == Int32Zero ifTrue: [Heaper BLAST: #NoBitCountLimit].
	^precision!
*/
}
public int actualHashForEqual() {
	return FHash.fastHashUInt32(myValue);
/*
udanax-top.st:35120:PrimIntValue methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^FHash fastHash.UInt32: myValue DOTasLong!
*/
}
public boolean isEqual(Heaper other) {
	return (other instanceof PrimIntValue) && (((PrimIntValue) other).asIntegerVar() == myValue);
/*
udanax-top.st:35124:PrimIntValue methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^(other isKindOf: PrimIntValue) and: [(other cast: PrimIntValue) asIntegerVar = myValue]!
*/
}
public PrimIntValue(int value) {
	super();
	myValue = value;
/*
udanax-top.st:35130:PrimIntValue methodsFor: 'protected: create'!
create: value {IntegerVar}
	super create.
	myValue := value.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myValue);
	oo.print(")");
/*
udanax-top.st:35137:PrimIntValue methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myValue << ')'!
*/
}
/**
 * @deprecated
 */
public int precision() {
	throw new PasseException();
/*
udanax-top.st:35143:PrimIntValue methodsFor: 'smalltalk: passe'!
{Int32} precision
	self passe "bitCount"!
*/
}
public PrimIntValue(Rcvr receiver) {
	super(receiver);
	myValue = receiver.receiveIntegerVar();
/*
udanax-top.st:35149:PrimIntValue methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myValue _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myValue);
/*
udanax-top.st:35153:PrimIntValue methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myValue.!
*/
}
public static PrimIntValue make(int value) {
	return new PrimIntValue(value);
/*
udanax-top.st:35166:PrimIntValue class methodsFor: 'create'!
make: value {IntegerVar}
	^ self create: value!
*/
}
/**
 * {IntegerVar CLIENT} asIntegerVar
 */
public static void infostProtocol() {
/*
udanax-top.st:35171:PrimIntValue class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IntegerVar CLIENT} asIntegerVar
"!
*/
}
public PrimIntValue() {
/*

Generated during transformation
*/
}
}
