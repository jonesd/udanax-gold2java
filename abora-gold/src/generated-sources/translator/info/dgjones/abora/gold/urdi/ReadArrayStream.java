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
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.ReadArrayStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;
import java.io.StringWriter;

public class ReadArrayStream extends XnReadStream {

	protected UInt8Array myBuffer;
	protected int myIndex;
/*
udanax-top.st:64907:
XnReadStream subclass: #ReadArrayStream
	instanceVariableNames: '
		myBuffer {UInt8Array}
		myIndex {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:64913:
(ReadArrayStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ReadArrayStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int getByte() {
	int result;
	result = myBuffer.uIntAt(myIndex);
	myIndex = myIndex + 1;
	return result;
/*
udanax-top.st:64918:ReadArrayStream methodsFor: 'accessing'!
{UInt8} getByte
	| result {UInt8} |
	result _ myBuffer uIntAt: myIndex.
	myIndex _ myIndex + 1.
	^result!
*/
}
public void putBack(int b) {
	if ( ! (myIndex > 0)) {
		throw new AboraAssertionException("Must have room");
	}
	myIndex = myIndex - 1;
	if ( ! ((myBuffer.uIntAt(myIndex)) == b)) {
		throw new AboraAssertionException("Must be same character");
	}
/*
udanax-top.st:64924:ReadArrayStream methodsFor: 'accessing'!
{void} putBack: b {UInt8}
	myIndex > UInt32Zero assert: 'Must have room'.
	myIndex := myIndex - 1.
	(myBuffer uIntAt: myIndex) == b assert: 'Must be same character'!
*/
}
public void refill() {
/*
udanax-top.st:64929:ReadArrayStream methodsFor: 'accessing'!
{void} refill!
*/
}
public ReadArrayStream(UInt8Array collection) {
	super();
	myBuffer = collection;
	myIndex = 0;
/*
udanax-top.st:64933:ReadArrayStream methodsFor: 'creation'!
create: collection {UInt8Array}
	super create.
	myBuffer _ collection.
	myIndex := UInt32Zero!
*/
}
public boolean end() {
	return myIndex >= myBuffer.count();
/*
udanax-top.st:64940:ReadArrayStream methodsFor: 'smalltalk: streams'!
{BooleanVar} atEnd
	^myIndex >= myBuffer count!
*/
}
public int next() {
	int result;
	result = myBuffer.uIntAt(myIndex);
	myIndex = myIndex + 1;
	/* Removed smalltalkOnly */
	return result;
/*
udanax-top.st:64943:ReadArrayStream methodsFor: 'smalltalk: streams'!
{UInt8} next
	| result {UInt8} |
	result _ myBuffer uIntAt: myIndex.
	myIndex _ myIndex + 1.
	[^Character char: result] smalltalkOnly.
	[^result] translateOnly.!
*/
}
public String peekAhead() {
	StringWriter stringWriter = new StringWriter();
	PrintWriter strm = new PrintWriter(stringWriter);
	for (int i = 0; i <= (Math.min(myBuffer.count() - myIndex, 100)); i ++ ) {
		strm.print(((char) ((myBuffer.uIntAt(myIndex + i)))));
	}
	return stringWriter.toString();
/*
udanax-top.st:64950:ReadArrayStream methodsFor: 'smalltalk: streams'!
{String} peekAhead
	^String streamContents: [:strm |
		0 to: (myBuffer count - myIndex min: 100)
		   do: [:i | strm nextPut: (Character char: (myBuffer uIntAt: myIndex + i))]]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myIndex);
	oo.print(", ");
	oo.print((myBuffer.count() - myIndex));
	oo.print(", \"");
	for (int i = 0; i < myIndex; i ++ ) {
		oo.print(((myBuffer.uIntAt(i))));
	}
	oo.print("<-|->");
	for (int j = myIndex; j < myBuffer.count(); j ++ ) {
		oo.print(((myBuffer.uIntAt(j))));
	}
	oo.print("\")");
/*
udanax-top.st:64957:ReadArrayStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	
	oo << self getCategory name << '(' << myIndex << ', ' << (myBuffer count - myIndex) << ', "'.
	Int32Zero almostTo: myIndex do: [:i {Int32} | oo DOTput: ((myBuffer uIntAt: i) basicCast: Character)].
	oo << '<-|->'.
	myIndex almostTo: myBuffer count do: [:j {Int32} | oo DOTput: ((myBuffer uIntAt: j) basicCast: Character)].
	oo << '")'!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:64967:ReadArrayStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:64969:ReadArrayStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public ReadArrayStream() {
/*

Generated during transformation
*/
}
public ReadArrayStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
