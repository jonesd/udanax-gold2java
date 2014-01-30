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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.WriteVariableArrayStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * WriteVariableArrayStream is used to put an unpredictable amount of data into a UInt8Array.
 * The array method returns the current state of the buffer.
 */
public class WriteVariableArrayStream extends XnWriteStream {

	protected UInt8Array myCollection;
	protected int myIndex;
/*
udanax-top.st:70559:
XnWriteStream subclass: #WriteVariableArrayStream
	instanceVariableNames: '
		myCollection {UInt8Array}
		myIndex {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:70565:
WriteVariableArrayStream comment:
'WriteVariableArrayStream is used to put an unpredictable amount of data into a UInt8Array.  The array method returns the current state of the buffer.'!
*/
/*
udanax-top.st:70567:
(WriteVariableArrayStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:70624:
WriteVariableArrayStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:70627:
(WriteVariableArrayStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WriteVariableArrayStream.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
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
udanax-top.st:70572:WriteVariableArrayStream methodsFor: 'accessing'!
{void} flush
	"We can't test for underflow because we deliberately overestimate 
	 the size when we don't have exact information on where things go."
	"myIndex == myMax assert: 'Must fill up the space'"!
*/
}
public void putByte(int bytex) {
	if (myIndex >= myCollection.count()) {
		myCollection = (UInt8Array) (myCollection.copyGrow(myIndex * 2));
	}
	myCollection.storeUInt(myIndex, bytex);
	myIndex = myIndex + 1;
/*
udanax-top.st:70577:WriteVariableArrayStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32}
	myIndex >= myCollection count ifTrue: [
		myCollection := (myCollection copyGrow: myIndex * 2) cast: UInt8Array].
	myCollection at: myIndex storeUInt: byte.
	myIndex := myIndex + 1!
*/
}
public void putData(UInt8Array array) {
	for (int i = 0; i < array.count(); i ++ ) {
		putByte((array.uIntAt(i)));
	}
/*
udanax-top.st:70584:WriteVariableArrayStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	Int32Zero almostTo: array count do: [:i {Int32} | self putByte: (array uIntAt: i)].!
*/
}
public void putStr(String string) {
	AboraSupport.smalltalkOnly();
	{
		for (int doIndex = 0; doIndex < string.length(); doIndex ++ ) {
			char ch = string.charAt(doIndex);
			putByte(ch);
		}
	}
	AboraSupport.translateOnly();
	{
		/* cxx:
	while (*string) {
		this->putByte(*string++);
	} */
		;
	}
/*
udanax-top.st:70588:WriteVariableArrayStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	[string do: [:ch {Character} | self putByte: ch uint8]] smalltalkOnly.
	
	["cxx:
	while (*string) {
		this->putByte(*string++);
	}"] translateOnly!
*/
}
public WriteVariableArrayStream(UInt8Array array) {
	super();
	myCollection = array;
	myIndex = 0;
/*
udanax-top.st:70598:WriteVariableArrayStream methodsFor: 'creation'!
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
udanax-top.st:70605:WriteVariableArrayStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myIndex << ', ' << (myCollection count - myIndex) << ', "'.
	UInt32Zero almostTo: myIndex do: [:i {Int32} | oo DOTput: ((myCollection uIntAt: i) basicCast: Character)].
	oo << '<-|->'.
	myIndex almostTo: myCollection count do: [:j  {Int32} | oo DOTput: ((myCollection uIntAt: j) basicCast: Character)].
	oo << '")'!
*/
}
public UInt8Array array() {
	return (UInt8Array) (myCollection.copy(myIndex));
/*
udanax-top.st:70614:WriteVariableArrayStream methodsFor: 'special'!
{UInt8Array} array
	^ (myCollection copy: myIndex) cast: UInt8Array!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:70619:WriteVariableArrayStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:70621:WriteVariableArrayStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static WriteVariableArrayStream make(int size) {
	return new WriteVariableArrayStream((UInt8Array.make(size)));
/*
udanax-top.st:70632:WriteVariableArrayStream class methodsFor: 'creation'!
make: size {Int32}
	^ self create: (UInt8Array make: size)!
*/
}
public WriteVariableArrayStream() {
/*

Generated during transformation
*/
}
public WriteVariableArrayStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
