/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.urdi;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.WriteArrayStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class WriteArrayStream extends XnWriteStream {

	protected UInt8Array myCollection;
	protected int myIndex;
/*
udanax-top.st:70398:
XnWriteStream subclass: #WriteArrayStream
	instanceVariableNames: '
		myCollection {UInt8Array}
		myIndex {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:70404:
(WriteArrayStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WriteArrayStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * We can't test for underflow because we deliberately overestimate
 * the size when we don't have exact information on where things go.
 */
public void flush() {
	/* myIndex == myMax assert: 'Must fill up the space' */
/*
udanax-top.st:70409:WriteArrayStream methodsFor: 'accessing'!
{void} flush
	"We can't test for underflow because we deliberately overestimate 
	 the size when we don't have exact information on where things go."
	"myIndex == myMax assert: 'Must fill up the space'"!
*/
}
public void putByte(int bytex) {
	if (myIndex >= myCollection.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
	}
	myCollection.storeUInt(myIndex, bytex);
	myIndex = myIndex + 1;
/*
udanax-top.st:70414:WriteArrayStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32}
	myIndex >= myCollection count ifTrue: [
		Heaper BLAST: #IndexOutOfBounds].
	myCollection at: myIndex storeUInt: byte.
	myIndex := myIndex + 1!
*/
}
public void putData(UInt8Array array) {
	if ((myIndex + array.count()) >= myCollection.count()) {
		throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
	}
	for (int i = 0; i < array.count(); i ++ ) {
		putByte((array.uIntAt(i)));
	}
/*
udanax-top.st:70421:WriteArrayStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	(myIndex + array count) >= myCollection count ifTrue: [
		Heaper BLAST: #IndexOutOfBounds].
	Int32Zero almostTo: array count do: [:i {Int32} | self putByte: (array uIntAt: i)].!
*/
}
public void putStr(String string) {
	for (int doIndex = 0; doIndex < string.length(); doIndex ++ ) {
		char ch = string.charAt(doIndex);
		putByte(ch);
	}
	/* Removed translateOnly */
/*
udanax-top.st:70427:WriteArrayStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	[string do: [:ch {Character} | self putByte: ch uint8]] smalltalkOnly.
	
	["cxx:
	while (*string) {
		this->putByte(*string++);
	}"] translateOnly!
*/
}
public WriteArrayStream(UInt8Array array) {
	super();
	myCollection = array;
	myIndex = 0;
/*
udanax-top.st:70437:WriteArrayStream methodsFor: 'creation'!
create: array {UInt8Array}
	super create.
	myCollection := array.
	myIndex := UInt32Zero.!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myIndex);
	oo.print(", ");
	oo.print((myCollection.count() - myIndex));
	oo.print(", \"");
	for (int i = 0; i < myIndex; i ++ ) {
		oo.print(((myCollection.uIntAt(i))));
	}
	oo.print("<-|->");
	for (int j = myIndex; j < myCollection.count(); j ++ ) {
		oo.print(((myCollection.uIntAt(j))));
	}
	oo.print("\")");
/*
udanax-top.st:70444:WriteArrayStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myIndex << ', ' << (myCollection count - myIndex) << ', "'.
	UInt32Zero almostTo: myIndex do: [:i {Int32} | oo DOTput: ((myCollection uIntAt: i) basicCast: Character)].
	oo << '<-|->'.
	myIndex almostTo: myCollection count do: [:j  {Int32} | oo DOTput: ((myCollection uIntAt: j) basicCast: Character)].
	oo << '")'!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:70453:WriteArrayStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:70455:WriteArrayStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public WriteArrayStream() {
/*

Generated during transformation
*/
}
public WriteArrayStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
