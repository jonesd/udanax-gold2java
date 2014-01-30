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
import info.dgjones.abora.gold.urdi.WriteArrayStream;
import info.dgjones.abora.gold.urdi.WriteMemStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class XnWriteStream extends Heaper {

/*
udanax-top.st:70237:
Heaper subclass: #XnWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:70241:
(XnWriteStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:70266:
XnWriteStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:70269:
(XnWriteStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XnWriteStream.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public void flush() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:70246:XnWriteStream methodsFor: 'accessing'!
{void} flush
	self subclassResponsibility!
*/
}
/**
 * These are UInt32 to avoid an unneeded mask op that the compiler generates
 */
public void putByte(int bytex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:70250:XnWriteStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32}
	"These are UInt32 to avoid an unneeded mask op that the compiler generates"
	self subclassResponsibility!
*/
}
public void putData(UInt8Array array) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:70254:XnWriteStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	self subclassResponsibility!
*/
}
public void putStr(String string) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:70257:XnWriteStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:70262:XnWriteStream methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Make a stream which writes into the given array
 */
public static XnWriteStream make(UInt8Array array) {
	return new WriteArrayStream(array);
/*
udanax-top.st:70274:XnWriteStream class methodsFor: 'creation'!
make: array {UInt8Array}
	"Make a stream which writes into the given array"
	^WriteArrayStream create: array!
*/
}
public static XnWriteStream make(UInt8Array dataP, int start, int count) {
	return WriteMemStream.make(dataP, start, count);
/*
udanax-top.st:70278:XnWriteStream class methodsFor: 'creation'!
make: dataP {UInt8 star} with: start {Int32} with: count {Int32}
	^WriteMemStream make: dataP with: start with: count!
*/
}
public XnWriteStream() {
/*

Generated during transformation
*/
}
public XnWriteStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
