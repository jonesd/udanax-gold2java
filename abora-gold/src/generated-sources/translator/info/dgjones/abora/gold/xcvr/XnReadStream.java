/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.ReadArrayStream;
import info.dgjones.abora.gold.urdi.ReadMemStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class XnReadStream extends Heaper {

/*
udanax-top.st:64856:
Heaper subclass: #XnReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64860:
(XnReadStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:64893:
XnReadStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64896:
(XnReadStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XnReadStream.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public int getByte() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64865:XnReadStream methodsFor: 'accessing'!
{UInt8} getByte
	self subclassResponsibility!
*/
}
/**
 * Pour data directly into a buffer.
 */
public void getBytes(UInt8Array buffer, int count, int start) {
	for (int i = start; i < start + count; i ++ ) {
		(buffer).put(i, getByte());
	}
/*
udanax-top.st:64868:XnReadStream methodsFor: 'accessing'!
{void} getBytes: buffer {void star} with: count {Int32} with: start {Int32 default: Int32Zero}
	"Pour data directly into a buffer."
	
	start almostTo: start + count do: [:i {Int32} |
		(buffer basicCast: Character star) at: i put: self getByte]!
*/
}
public void putBack(int c) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64874:XnReadStream methodsFor: 'accessing'!
{void} putBack: c {UInt8}
	self subclassResponsibility!
*/
}
public void refill() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64877:XnReadStream methodsFor: 'accessing'!
{void} refill
	self subclassResponsibility!
*/
}
/**
 * Pour data directly into a buffer.
 */
public void getBytes(UInt8Array buffer, int count) {
	getBytes(buffer, count, 0);
/*
udanax-top.st:64882:XnReadStream methodsFor: 'smalltalk: defaults'!
{void} getBytes: buffer {void star} with: count {Int32}
	"Pour data directly into a buffer."
	
	self getBytes: buffer with: count with: Int32Zero!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:64889:XnReadStream methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static XnReadStream make(UInt8Array collection) {
	return new ReadArrayStream(collection);
/*
udanax-top.st:64901:XnReadStream class methodsFor: 'creation'!
make: collection {UInt8Array}
	^ReadArrayStream create: collection.!
*/
}
public static XnReadStream make(UInt8Array dataP, int start, int count) {
	return ReadMemStream.make(dataP, start, count);
/*
udanax-top.st:64904:XnReadStream class methodsFor: 'creation'!
make: dataP {UInt8 star} with: start {Int32} with: count {Int32}
	^ReadMemStream make: dataP with: start with: count!
*/
}
public XnReadStream() {
/*

Generated during transformation
*/
}
public XnReadStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public boolean end() {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
