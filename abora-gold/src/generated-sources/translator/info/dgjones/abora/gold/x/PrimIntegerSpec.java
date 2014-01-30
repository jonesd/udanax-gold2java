/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.x;

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimIntegerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class PrimIntegerSpec extends PrimSpec {

	protected int myBitCount;
	protected boolean amSigned;
	protected int myMin;
	protected int myMax;
/*
udanax-top.st:34450:
PrimSpec subclass: #PrimIntegerSpec
	instanceVariableNames: '
		myBitCount {Int32}
		amSigned {BooleanVar}
		myMin {Int32}
		myMax {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34458:
(PrimIntegerSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
/*
udanax-top.st:34613:
PrimIntegerSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34616:
(PrimIntegerSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimIntegerSpec.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "xpp"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * How many bits, or zero if it is unlimited
 */
public int bitCount() {
	return myBitCount;
/*
udanax-top.st:34463:PrimIntegerSpec methodsFor: 'accessing'!
{Int32 INLINE} bitCount
	"How many bits, or zero if it is unlimited"
	
	^myBitCount!
*/
}
/**
 * A spec whose range of values contains both ranges
 */
public PrimIntegerSpec combine(PrimIntegerSpec other) {
	if (this == other) {
		return this;
	}
	if (myBitCount == 0) {
		return this;
	}
	if (other.bitCount() == 0) {
		return other;
	}
	if (myBitCount < other.bitCount()) {
		return other;
	}
	if (myBitCount > other.bitCount()) {
		return this;
	}
	if (amSigned == other.isSigned()) {
		return this;
	}
	/* here we get ad hoc since we need to expand to the next larger size */
	if (myBitCount == 8) {
		return PrimSpec.int32();
	}
	return PrimSpec.integerVar();
/*
udanax-top.st:34468:PrimIntegerSpec methodsFor: 'accessing'!
{PrimIntegerSpec} combine: other {PrimIntegerSpec}
	"A spec whose range of values contains both ranges"
	
	self == other ifTrue: [^self].
	myBitCount == Int32Zero ifTrue: [^self].
	other bitCount == Int32Zero ifTrue: [^other].
	myBitCount < other bitCount ifTrue: [^other].
	myBitCount > other bitCount ifTrue: [^self].
	amSigned == other isSigned ifTrue: [^self].
	"here we get ad hoc since we need to expand to the next larger size"
	myBitCount == 8 ifTrue: [^PrimSpec int32].
	^PrimSpec integerVar!
*/
}
/**
 * Whether it allows negative values
 */
public boolean isSigned() {
	return amSigned;
/*
udanax-top.st:34481:PrimIntegerSpec methodsFor: 'accessing'!
{BooleanVar INLINE} isSigned
	"Whether it allows negative values"
	
	^amSigned!
*/
}
public PrimIntegerSpec(Category primClass, int bitCount, boolean isSigned) {
	super(primClass);
	myBitCount = bitCount;
	amSigned = isSigned;
	if (myBitCount != -1) {
		if (amSigned) {
			myMin = 1 << (myBitCount - 1);
			myMax = ~ myMin;
			/* Removed smalltalkOnly */
			/* Removed translateOnly */
		}
		else {
			myMin = 0;
			myMax = ~ ((( ~ 0) << (myBitCount - 1)) << 1);
			/* Removed smalltalkOnly */
			/* Removed translateOnly */
		}
	}
/*
udanax-top.st:34488:PrimIntegerSpec methodsFor: 'create'!
create: primClass {Category} with: bitCount {Int32} with: isSigned {BooleanVar}
	super create: primClass.
	myBitCount := bitCount.
	amSigned := isSigned.
	myBitCount ~~ -1 ifTrue:
		[amSigned ifTrue:
			[[myMin := (2 raisedTo: myBitCount - 1) negated.
			myMax := myMin negated - 1] smalltalkOnly.
			'myMin = 1 << (myBitCount - 1);
			myMax = ~myMin;' translateOnly]
		ifFalse:
			[[myMin := Int32Zero.
			myMax := (2 raisedTo: myBitCount) - 1] smalltalkOnly.
			'myMin = Int32Zero;
			/- the shift is done in two steps to avoid five-bit truncation on SPARCs -/
			myMax = ~(((~Int32Zero) << (myBitCount - 1)) << 1);' translateOnly]]!
*/
}
public int actualHashForEqual() {
	int signPart;
	if (amSigned) {
		signPart = 255;
	}
	else {
		signPart = 0;
	}
	return (getCategory().hashForEqual() ^ (FHash.fastHashUInt32(myBitCount))) ^ signPart;
/*
udanax-top.st:34508:PrimIntegerSpec methodsFor: 'testing'!
{UInt32} actualHashForEqual
	| signPart {UInt32} |
	amSigned ifTrue: [signPart _ 255] ifFalse: [signPart _ UInt32Zero].
	^(self getCategory hashForEqual
		bitXor: (FHash fastHash.UInt32: myBitCount))
		bitXor: signPart!
*/
}
/**
 * Whether this spec can hold the given value
 */
public boolean canHold(int value) {
	return myBitCount == -1 || (value >= myMin && (value <= myMax));
/*
udanax-top.st:34515:PrimIntegerSpec methodsFor: 'testing'!
{BooleanVar} canHold: value {IntegerVar}
	"Whether this spec can hold the given value"
	
	[^myBitCount = -1 or: [value >= myMin and: [value <= myMax]]] smalltalkOnly.
	'if (myBitCount = -1) {
		return TRUE;
	} else if (amSigned) {
		return value >= myMin && value <= myMax;
	} else {
		return (unsigned) value.asLong () >= (unsigned) myMin
			&& (unsigned) value.asLong () <= (unsigned) myMax;
	}' translateOnly!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof PrimIntegerSpec) {
		PrimIntegerSpec spec = (PrimIntegerSpec) other;
		return myBitCount == spec.bitCount() && (amSigned == spec.isSigned());
	}
	else {
		return false;
	}
/*
udanax-top.st:34528:PrimIntegerSpec methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: PrimIntegerSpec into: [ :spec |
		^myBitCount = spec bitCount
			and: [amSigned == spec isSigned]]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
/**
 * Make an array initialized to zero values
 */
public PrimArray array(int count) {
	if (this == ((Heaper) PrimSpec.uInt32())) {
		return Int32Array.make(count);
	}
	if (this == ((Heaper) PrimSpec.uInt8())) {
		return UInt8Array.make(count);
	}
	if (this == ((Heaper) PrimSpec.int32())) {
		return Int32Array.make(count);
	}
	if (this == ((Heaper) PrimSpec.integerVar())) {
		return IntegerVarArray.zeros(count);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34539:PrimIntegerSpec methodsFor: 'making'!
{PrimArray} array: count {Int32 default: Int32Zero}
	"Make an array initialized to zero values"
	
	[self == (PrimSpec uInt32 basicCast: Heaper star) ifTrue: [^UInt32Array make: count].
	self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array make: count].
	self == (PrimSpec int32 basicCast: Heaper star) ifTrue: [^Int32Array make: count].
	self == (PrimSpec integerVar basicCast: Heaper star) ifTrue: [^IntegerVarArray zeros: count].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: count] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * Make an array with the values at the given address
 */
public PrimArray arrayFromBuffer(int count, PtrArray buffer) {
	if (this == ((Heaper) PrimSpec.uInt32())) {
		return Int32Array.make(count, buffer);
	}
	if (this == ((Heaper) PrimSpec.uInt8())) {
		return UInt8Array.make(count, buffer);
	}
	if (this == ((Heaper) PrimSpec.int32())) {
		return Int32Array.make(count, buffer);
	}
	if (this == ((Heaper) PrimSpec.integerVar())) {
		return IntegerVarArray.make(count, buffer);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34550:PrimIntegerSpec methodsFor: 'making'!
{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
	"Make an array with the values at the given address"
	
	[self == (PrimSpec uInt32 basicCast: Heaper star) ifTrue: [^UInt32Array make: count with: buffer].
	self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array make: count with: buffer].
	self == (PrimSpec int32 basicCast: Heaper star) ifTrue: [^Int32Array make: count with: buffer].
	self == (PrimSpec integerVar basicCast: Heaper star) ifTrue: [^IntegerVarArray make: count with: buffer].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: count with: buffer] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * Make an array the contents of the string
 */
public PrimIntegerArray string(String string) {
	if (this == ((Heaper) PrimSpec.uInt8())) {
		return UInt8Array.string(string);
	}
	throw new UnimplementedException();
/*
udanax-top.st:34561:PrimIntegerSpec methodsFor: 'making'!
{PrimIntegerArray} string: string {char star}
	"Make an array the contents of the string"
	
	self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array string: string].
	self unimplemented.
	^NULL "fodder"!
*/
}
/**
 * A boxed integer value
 */
public PrimIntValue value(int number) {
	return PrimIntValue.make(number);
/*
udanax-top.st:34568:PrimIntegerSpec methodsFor: 'making'!
{PrimIntValue INLINE} value: number {IntegerVar}
	"A boxed integer value"
	
	^PrimIntValue make: number!
*/
}
/**
 * Make a copy of an array with a different representation size. The arguments are the same
 * as in PrimArray::copy.
 */
public PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset) {
	if (this == ((Heaper) PrimSpec.uInt32())) {
		return Int32Array.make(size, array, start, count, offset);
	}
	if (this == ((Heaper) PrimSpec.uInt8())) {
		return UInt8Array.make(size, array, start, count, offset);
	}
	if (this == ((Heaper) PrimSpec.int32())) {
		return Int32Array.make(size, array, start, count, offset);
	}
	if (this == ((Heaper) PrimSpec.integerVar())) {
		return IntegerVarArray.make(size, array, start, count, offset);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34575:PrimIntegerSpec methodsFor: 'private: making'!
{PrimArray} privateCopy: array {PrimArray}
	with: size {Int32 default: -1}
	with: start {Int32 default: Int32Zero}
	with: count {Int32 default: -1}
	with: offset {Int32 default: Int32Zero}
	"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
	
	[self == (PrimSpec uInt32 basicCast: Heaper star) ifTrue: [^UInt32Array make: size with: array with: start with: count with: offset].
	self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array make: size with: array with: start with: count with: offset].
	self == (PrimSpec int32 basicCast: Heaper star) ifTrue: [^Int32Array make: size with: array with: start with: count with: offset].
	self == (PrimSpec integerVar basicCast: Heaper star) ifTrue: [^IntegerVarArray make: size with: array with: start with: count with: offset].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: size with: array with: start with: count with: offset] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * @deprecated
 */
public int precision() {
	throw new PasseException();
/*
udanax-top.st:34592:PrimIntegerSpec methodsFor: 'smalltalk: passe'!
{Int32} precision
	self passe "bitCount"!
*/
}
public PrimIntegerSpec(Rcvr receiver) {
	super(receiver);
	myBitCount = receiver.receiveInt32();
	amSigned = receiver.receiveBooleanVar();
	myMin = receiver.receiveInt32();
	myMax = receiver.receiveInt32();
/*
udanax-top.st:34598:PrimIntegerSpec methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myBitCount _ receiver receiveInt32.
	amSigned _ receiver receiveBooleanVar.
	myMin _ receiver receiveInt32.
	myMax _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendInt32(myBitCount);
	xmtr.sendBooleanVar(amSigned);
	xmtr.sendInt32(myMin);
	xmtr.sendInt32(myMax);
/*
udanax-top.st:34605:PrimIntegerSpec methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: myBitCount.
	xmtr sendBooleanVar: amSigned.
	xmtr sendInt32: myMin.
	xmtr sendInt32: myMax.!
*/
}
/**
 * {BooleanVar CLIENT} isSigned
 * {Int32 CLIENT} precision
 * {PrimIntegerArray CLIENT} string: string {char star}
 * {PrimInteger CLIENT} value: number {IntegerVar}
 */
public static void infostProtocol() {
/*
udanax-top.st:34621:PrimIntegerSpec class methodsFor: 'smalltalk: system'!
info.stProtocol
"{BooleanVar CLIENT} isSigned
{Int32 CLIENT} precision
{PrimIntegerArray CLIENT} string: string {char star}
{PrimInteger CLIENT} value: number {IntegerVar}
"!
*/
}
public PrimIntegerSpec() {
/*

Generated during transformation
*/
}
}
