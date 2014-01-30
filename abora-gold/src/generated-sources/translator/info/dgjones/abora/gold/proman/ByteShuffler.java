/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.ByteShuffler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Instances shuffle bytes to convert between byte sexes.  Subclasses are defined for each of
 * the various transformations.
 */
public class ByteShuffler extends Heaper {

/*
udanax-top.st:12941:
Heaper subclass: #ByteShuffler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:12945:
ByteShuffler comment:
'Instances shuffle bytes to convert between byte sexes.  Subclasses are defined for each of the various transformations.'!
*/
/*
udanax-top.st:12947:
(ByteShuffler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ByteShuffler.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return a shuffler that inverts the receiver's shuffler.  This will typically be the same
 * transformation.
 */
public ByteShuffler inverse() {
	return this;
/*
udanax-top.st:12952:ByteShuffler methodsFor: 'shuffle'!
{ByteShuffler} inverse
	"Return a shuffler that inverts the receiver's shuffler.  This will typically be the same transformation."
	
	^self!
*/
}
/**
 * Go from one byte sex to another for representing numbers of the specified precision.
 */
public void shuffle(int precision, UInt8Array buffer, int size) {
	if (precision == 8) {
		return ;
	}
	if (precision == 16) {
		shuffle16(buffer, size);
		return ;
	}
	if (precision == 32) {
		shuffle32(buffer, size);
		return ;
	}
	if (precision == 64) {
		shuffle64(buffer, size);
		return ;
	}
	throw new AboraRuntimeException(AboraRuntimeException.BAD_PRECISION);
/*
udanax-top.st:12957:ByteShuffler methodsFor: 'shuffle'!
{void} shuffle: precision {Int32} with: buffer {void star} with: size {Int32}
	"Go from one byte sex to another for representing numbers of the specified precision."
	
	precision == 8 ifTrue: [^VOID].
	precision == 16 ifTrue: [self shuffle16: buffer with: size. ^VOID].
	precision == 32 ifTrue: [self shuffle32: buffer with: size. ^VOID].
	precision == 64 ifTrue: [self shuffle64: buffer with: size. ^VOID].
	Heaper BLAST: #BadPrecision!
*/
}
/**
 * Go from one byte sex to another for representing 16 bit numbers.
 */
public void shuffle16(UInt8Array buffer, int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12968:ByteShuffler methodsFor: 'private: shuffle'!
{void} shuffle16: buffer {void star} with: count {Int32}
	"Go from one byte sex to another for representing 16 bit numbers."
	
	self subclassResponsibility!
*/
}
/**
 * Go from one byte sex to another for representing 32 bit numbers.
 */
public void shuffle32(UInt8Array buffer, int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12973:ByteShuffler methodsFor: 'private: shuffle'!
{void} shuffle32: buffer {void star} with: count {Int32}
	"Go from one byte sex to another for representing 32 bit numbers."
	
	self subclassResponsibility!
*/
}
/**
 * Go from one byte sex to another for representing 64 bit numbers.
 */
public void shuffle64(UInt8Array buffer, int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:12978:ByteShuffler methodsFor: 'private: shuffle'!
{void} shuffle64: buffer {void star} with: count {Int32}
	"Go from one byte sex to another for representing 64 bit numbers."
	
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:12985:ByteShuffler methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public ByteShuffler() {
/*

Generated during transformation
*/
}
public ByteShuffler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public void shuffle(int precision, PrimArray buffer, int size) {
	shuffle(precision, (UInt8Array) buffer, size);
/*

Generated during transformation: AddMethod
*/
}
}
