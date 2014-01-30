/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.urdi;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.CountStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class CountStream extends XnWriteStream {

	protected int mySize;
	protected static InstanceCache SomeStreams;
/*
udanax-top.st:70281:
XnWriteStream subclass: #CountStream
	instanceVariableNames: 'mySize {Int32}'
	classVariableNames: 'SomeStreams {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:70285:
(CountStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:70326:
CountStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:70329:
(CountStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CountStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CountStream() {
	super();
	mySize = 0;
/*
udanax-top.st:70290:CountStream methodsFor: 'create'!
create
	super create.
	mySize _ Int32Zero!
*/
}
public void destroy() {
	if ( ! (SomeStreams.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:70294:CountStream methodsFor: 'create'!
{void} destroy
	(SomeStreams store: self) ifFalse: [super destroy]!
*/
}
/**
 * Must be a no-op since Xmtrs flush when done.
 */
public void flush() {
/*
udanax-top.st:70299:CountStream methodsFor: 'accessing'!
{void} flush
	"Must be a no-op since Xmtrs flush when done."!
*/
}
public void putByte(int bytex) {
	mySize = mySize + 1;
/*
udanax-top.st:70302:CountStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32 unused}
	mySize _ mySize + 1!
*/
}
public void putData(UInt8Array array) {
	mySize = mySize + array.count();
/*
udanax-top.st:70305:CountStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	mySize _ mySize + array count!
*/
}
public void putStr(String string) {
	mySize = mySize + (string.length());
/*
udanax-top.st:70308:CountStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	mySize _ mySize + (String strlen: string)!
*/
}
public int size() {
	return mySize;
/*
udanax-top.st:70311:CountStream methodsFor: 'accessing'!
{Int32} size
	^mySize!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(mySize);
	oo.print(")");
/*
udanax-top.st:70316:CountStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << mySize << ')'.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:70321:CountStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:70323:CountStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void initTimeNonInherited() {
	SomeStreams = InstanceCache.make(16);
/*
udanax-top.st:70334:CountStream class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeStreams := InstanceCache make: 16!
*/
}
public static void linkTimeNonInherited() {
	SomeStreams = null;
/*
udanax-top.st:70337:CountStream class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeStreams := NULL!
*/
}
public static XnWriteStream make() {
	Heaper result;
	result = SomeStreams.fetch();
	if (result == null) {
		return new CountStream();
	}
	else {
		return 
		/* TODO newBecome */
		new CountStream();
	}
/*
udanax-top.st:70342:CountStream class methodsFor: 'creation'!
{XnWriteStream} make
	| result {Heaper} |
	result := SomeStreams fetch.
	result == NULL
		ifTrue: [^self create]
		ifFalse: [^(self new.Become: result) create]!
*/
}
public CountStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
