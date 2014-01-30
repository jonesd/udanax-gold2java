/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.x;

import info.dgjones.abora.gold.collection.basic.IEEE32Array;
import info.dgjones.abora.gold.collection.basic.IEEE64Array;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.UnimplementedException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.IEEE128;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimFloatSpec;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Specifies different precisions and representations of floating point numbers.
 */
public class PrimFloatSpec extends PrimSpec {

	protected int myBitCount;
/*
udanax-top.st:34328:
PrimSpec subclass: #PrimFloatSpec
	instanceVariableNames: 'myBitCount {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34332:
PrimFloatSpec comment:
'Specifies different precisions and representations of floating point numbers.'!
*/
/*
udanax-top.st:34334:
(PrimFloatSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
/*
udanax-top.st:34436:
PrimFloatSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34439:
(PrimFloatSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimFloatSpec.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "xpp"}));
/*

Generated during transformation: AddMethod
*/
}
/**
 * How many total bits per value
 */
public int bitCount() {
	return myBitCount;
/*
udanax-top.st:34339:PrimFloatSpec methodsFor: 'accessing'!
{Int32 INLINE} bitCount
	"How many total bits per value"
	
	^myBitCount!
*/
}
public PrimFloatSpec(Category primClass, int bitCount) {
	super(primClass);
	myBitCount = bitCount;
/*
udanax-top.st:34346:PrimFloatSpec methodsFor: 'create'!
create: primClass {Category} with: bitCount {Int32}
	super create: primClass.
	myBitCount := bitCount.!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual() ^ (FHash.fastHashUInt32(myBitCount));
/*
udanax-top.st:34353:PrimFloatSpec methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual
		bitXor: (FHash fastHash.UInt32: myBitCount)!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof PrimFloatSpec) {
		PrimFloatSpec spec = (PrimFloatSpec) other;
		return myBitCount == spec.bitCount();
	}
	else {
		return false;
	}
/*
udanax-top.st:34358:PrimFloatSpec methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: PrimFloatSpec into: [ :spec |
		^myBitCount = spec bitCount]
	others:
		[^false].
	^ false "compiler fodder"!
*/
}
/**
 * Make a copy of an array with a different representation size. The arguments are the same
 * as in PrimArray::copy.
 */
public PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset) {
	Eric.thingToDo();
	/* Add case for generic floating point--essentially PtrArray of PrimFloat */
	if (this == ((Heaper) PrimSpec.iEEE64())) {
		return IEEE64Array.make(size, array, start, count, offset);
	}
	if (this == ((Heaper) PrimSpec.iEEE32())) {
		return IEEE32Array.make(size, array, start, count, offset);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34368:PrimFloatSpec methodsFor: 'private: making'!
{PrimArray} privateCopy: array {PrimArray}
	with: size {Int32 default: -1}
	with: start {Int32 default: Int32Zero}
	with: count {Int32 default: -1}
	with: offset {Int32 default: Int32Zero}
	"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
	
	[Eric thingToDo. "Add case for generic floating point--essentially PtrArray of PrimFloat"
	self == (PrimSpec iEEE64 basicCast: Heaper star) ifTrue: [^IEEE64Array make: size with: array with: start with: count with: offset].
	self == (PrimSpec iEEE32 basicCast: Heaper star) ifTrue: [^IEEE32Array make: size with: array with: start with: count with: offset].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: size with: start with: count with: offset] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * Make an array initialized to zero values
 */
public PrimArray array(int count) {
	Eric.thingToDo();
	/* Add case for generic floating point--essentially PtrArray of PrimFloat */
	if (this == ((Heaper) PrimSpec.iEEE64())) {
		return IEEE64Array.make(count);
	}
	if (this == ((Heaper) PrimSpec.iEEE32())) {
		return IEEE32Array.make(count);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34384:PrimFloatSpec methodsFor: 'making'!
{PrimArray} array: count {Int32 default: Int32Zero}
	"Make an array initialized to zero values"
	[Eric thingToDo. "Add case for generic floating point--essentially PtrArray of PrimFloat"
	self == (PrimSpec iEEE64 basicCast: Heaper star) ifTrue: [^IEEE64Array make: count].
	self == (PrimSpec iEEE32 basicCast: Heaper star) ifTrue: [^IEEE32Array make: count].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: count] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * Make an array with the values at the given address
 */
public PrimArray arrayFromBuffer(int count, PtrArray buffer) {
	/* Generic case of unspecified size can't be handled here. */
	if (this == ((Heaper) PrimSpec.iEEE64())) {
		return IEEE64Array.make(count, buffer);
	}
	if (this == ((Heaper) PrimSpec.iEEE32())) {
		return IEEE32Array.make(count, buffer);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34394:PrimFloatSpec methodsFor: 'making'!
{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
	"Make an array with the values at the given address"
	
	["Generic case of unspecified size can't be handled here."
	self == (PrimSpec iEEE64 basicCast: Heaper star) ifTrue: [^IEEE64Array make: count with: buffer].
	self == (PrimSpec iEEE32 basicCast: Heaper star) ifTrue: [^IEEE32Array make: count with: buffer].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: count with: buffer] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * A boxed floating point value from a large precision number
 */
public PrimFloatValue preciseValue(IEEE128 number) {
	/* myBitCount = 32 ifTrue: [^PrimIEEE32 make: self with: number].
	myBitCount = 64 ifTrue: [^PrimIEEE64 make: self with: number] */
	throw new UnimplementedException();
/*
udanax-top.st:34404:PrimFloatSpec methodsFor: 'making'!
{PrimFloatValue} preciseValue: number {IEEE128}
	"A boxed floating point value from a large precision number"
	
	"myBitCount = 32 ifTrue: [^PrimIEEE32 make: self with: number].
	myBitCount = 64 ifTrue: [^PrimIEEE64 make: self with: number]"
	self unimplemented.
	^NULL "fodder"!
*/
}
/**
 * A boxed floating point value
 */
public PrimFloatValue value(double number) {
	if (myBitCount == 32) {
		return PrimIEEE32.make((float) number);
	}
	if (myBitCount == 64) {
		return PrimIEEE64.make(number);
	}
	return null;
/*
udanax-top.st:34412:PrimFloatSpec methodsFor: 'making'!
{PrimFloatValue} value: number {IEEE64}
	"A boxed floating point value"
	
	myBitCount = 32 ifTrue: [^PrimIEEE32 make: number].
	myBitCount = 64 ifTrue: [^PrimIEEE64 make: number].
	^NULL "fodder"!
*/
}
/**
 * @deprecated
 */
public int precision() {
	throw new PasseException();
/*
udanax-top.st:34421:PrimFloatSpec methodsFor: 'smalltalk: passe'!
{Int32} precision
	self passe "bitCount"!
*/
}
public PrimFloatSpec(Rcvr receiver) {
	super(receiver);
	myBitCount = receiver.receiveInt32();
/*
udanax-top.st:34427:PrimFloatSpec methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myBitCount _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendInt32(myBitCount);
/*
udanax-top.st:34431:PrimFloatSpec methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: myBitCount.!
*/
}
/**
 * {PrimFloat CLIENT} preciseValue: number {IEEE128}
 * {Int32 CLIENT} precision
 * {PrimFloat CLIENT} value: number {IEEE64}
 */
public static void infostProtocol() {
/*
udanax-top.st:34444:PrimFloatSpec class methodsFor: 'smalltalk: system'!
info.stProtocol
"{PrimFloat CLIENT} preciseValue: number {IEEE128}
{Int32 CLIENT} precision
{PrimFloat CLIENT} value: number {IEEE64}
"!
*/
}
public PrimFloatSpec() {
/*

Generated during transformation
*/
}
}
