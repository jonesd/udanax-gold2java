/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.x;

import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.SharedPtrArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimPointerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Describes a kind of primitive pointer array
 */
public class PrimPointerSpec extends PrimSpec {

/*
udanax-top.st:34628:
PrimSpec subclass: #PrimPointerSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34632:
PrimPointerSpec comment:
'Describes a kind of primitive pointer array'!
*/
/*
udanax-top.st:34634:
(PrimPointerSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimPointerSpec.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "xpp"}));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual() ^ arrayClass().hashForEqual();
/*
udanax-top.st:34639:PrimPointerSpec methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual bitXor: self arrayClass hashForEqual!
*/
}
public boolean isEqual(Heaper other) {
	return (other instanceof PrimPointerSpec) && (arrayClass() == ((PrimPointerSpec) other).arrayClass());
/*
udanax-top.st:34643:PrimPointerSpec methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	^(other isKindOf: PrimPointerSpec) and: [self arrayClass == (other cast: PrimPointerSpec) arrayClass]!
*/
}
/**
 * Make a copy of an array with a different representation size. The arguments are the same
 * as in PrimArray::copy.
 */
public PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset) {
	if (this == ((Heaper) PrimSpec.pointer())) {
		return PtrArray.make(size, array, start, count, offset);
	}
	if (this == ((Heaper) PrimSpec.sharedPointer())) {
		return (SharedPtrArray) SharedPtrArray.make(size, array, start, count, offset);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34649:PrimPointerSpec methodsFor: 'private: making'!
{PrimArray} privateCopy: array {PrimArray}
	with: size {Int32 default: -1}
	with: start {Int32 default: Int32Zero}
	with: count {Int32 default: -1}
	with: offset {Int32 default: Int32Zero}
	"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
	
	[self == (PrimSpec pointer basicCast: Heaper star) ifTrue: [^PtrArray make: size with: array with: start with: count with: offset].
	self == (PrimSpec sharedPointer basicCast: Heaper star) ifTrue: [^SharedPtrArray make: size with: array with: start with: count with: offset].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: size with: start with: count with: offset] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
public PrimPointerSpec(Category primClass) {
	super(primClass);
/*
udanax-top.st:34665:PrimPointerSpec methodsFor: 'create'!
create: primClass {Category}
	super create: primClass.!
*/
}
/**
 * Make an array initialized to null values
 */
public PrimArray array(int count) {
	if (this == ((Heaper) PrimSpec.pointer())) {
		return PtrArray.nulls(count);
	}
	if (this == ((Heaper) PrimSpec.sharedPointer())) {
		return (SharedPtrArray) SharedPtrArray.make(count);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34670:PrimPointerSpec methodsFor: 'making'!
{PrimArray} array: count {Int32 default: Int32Zero}
	"Make an array initialized to null values"
	[self == (PrimSpec pointer basicCast: Heaper star) ifTrue: [^PtrArray nulls: count].
	self == (PrimSpec sharedPointer basicCast: Heaper star) ifTrue: [^SharedPtrArray make: count].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: count] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
/**
 * Make an array with the values at the given address
 */
public PrimArray arrayFromBuffer(int count, PtrArray buffer) {
	if (this == ((Heaper) PrimSpec.pointer())) {
		return PtrArray.make(count, buffer);
	}
	if (this == ((Heaper) PrimSpec.sharedPointer())) {
		return (SharedPtrArray) SharedPtrArray.make(count, buffer);
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRIM_SPEC);
/*
udanax-top.st:34679:PrimPointerSpec methodsFor: 'making'!
{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
	"Make an array with the values at the given address"
	[self == (PrimSpec pointer basicCast: Heaper star) ifTrue: [^PtrArray make: count with: buffer].
	self == (PrimSpec sharedPointer basicCast: Heaper star) ifTrue: [^SharedPtrArray make: count with: buffer].
	Heaper BLAST: #BadPrimSpec] translateOnly.
	[^myClass create: count with: buffer] smalltalkOnly.
	^ NULL "compiler fodder"!
*/
}
public PrimPointerSpec(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:34690:PrimPointerSpec methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:34693:PrimPointerSpec methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public PrimPointerSpec() {
/*

Generated during transformation
*/
}
}
