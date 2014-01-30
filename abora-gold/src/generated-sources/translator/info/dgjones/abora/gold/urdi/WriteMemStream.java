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
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.WriteMemStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class WriteMemStream extends XnWriteStream {

	protected UInt8Array myCollection;
	protected int myStart;
	protected int myIndex;
	protected int myMax;
	protected static InstanceCache SomeStreams;
/*
udanax-top.st:70457:
XnWriteStream subclass: #WriteMemStream
	instanceVariableNames: '
		myCollection {UInt8 star}
		myStart {Int32}
		myIndex {Int32}
		myMax {Int32}'
	classVariableNames: 'SomeStreams {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:70465:
(WriteMemStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:70536:
WriteMemStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:70539:
(WriteMemStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WriteMemStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
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
udanax-top.st:70470:WriteMemStream methodsFor: 'accessing'!
{void} flush
	"We can't test for underflow because we deliberately overestimate 
	 the size when we don't have exact information on where things go."
	"myIndex == myMax assert: 'Must fill up the space'"!
*/
}
public void putByte(int bytex) {
	if ( ! (myIndex < myMax)) {
		throw new AboraAssertionException("Overrun");
	}
	myCollection.put(myIndex, bytex);
	myIndex = myIndex + 1;
/*
udanax-top.st:70475:WriteMemStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32}
	myIndex < myMax assert: 'Overrun'.
	myCollection at: myIndex put: byte.
	myIndex _ myIndex + 1!
*/
}
public void putData(UInt8Array array) {
	int index;
	int count;
	index = myIndex;
	count = array.count();
	if ( ! (index + count < myMax)) {
		throw new AboraAssertionException("Overrun");
	}
	for (int i = 0; i < count; i ++ ) {
		myCollection.put(index + i, (array.uIntAt(i)));
	}
	myIndex = index + count;
/*
udanax-top.st:70480:WriteMemStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	| index {Int32} count {Int32} |
	index := myIndex.
	count := array count.
	index + count < myMax assert: 'Overrun'.
	Int32Zero almostTo: count do: [:i {Int32} | myCollection at: index + i put: (array uIntAt: i)].
	myIndex := index + count.!
*/
}
public void putStr(String string) {
	for (int doIndex = 0; doIndex < string.length(); doIndex ++ ) {
		char ch = string.charAt(doIndex);
		putByte(ch);
	}
	/* Removed translateOnly */
/*
udanax-top.st:70488:WriteMemStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	[string do: [:ch {Character} | self putByte: ch uint8]] smalltalkOnly.
	
	'
	while (*string) {
		this->putByte(*string++);
	}' translateOnly!
*/
}
public WriteMemStream(UInt8Array collection, int index, int count) {
	super();
	if ( ! (index >= 0)) {
		throw new AboraAssertionException("Must start inside buffer");
	}
	myCollection = collection;
	myIndex = index;
	myMax = myIndex + count;
	myStart = index;
/*
udanax-top.st:70498:WriteMemStream methodsFor: 'creation'!
create: collection {UInt8 star} with: index {Int32} with: count {Int32}
	super create. 
	index >= Int32Zero assert: 'Must start inside buffer'.
	myCollection _ collection.
	myIndex _ index.
	myMax _ myIndex + count.
	myStart _ index!
*/
}
public void destroy() {
	if ( ! (SomeStreams.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:70506:WriteMemStream methodsFor: 'creation'!
{void} destroy
	(SomeStreams store: self) ifFalse: [super destroy]!
*/
}
public PrimArray contents() {
	return myCollection.copy(myMax - myStart, myStart);
/*
udanax-top.st:70511:WriteMemStream methodsFor: 'debugging'!
{char star} contents
	[^myCollection copy: myMax - myStart with: myStart] smalltalkOnly.
	'char * result = new char [myMax - myStart];
	MEMMOVE(result, myCollection + myStart, myMax - myStart);
	return result;' translateOnly.!
*/
}
public int size() {
	return myIndex - myStart;
/*
udanax-top.st:70517:WriteMemStream methodsFor: 'debugging'!
{Int32} size
	^myIndex - myStart!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print((myIndex - myStart));
	oo.print(", ");
	oo.print((myMax - myIndex));
	oo.print(", \"");
	for (int i = myStart; i < myIndex; i ++ ) {
		oo.print(((myCollection.at(i))));
	}
	oo.print("<-|->");
	for (int j = myIndex; j < myMax; j ++ ) {
		oo.print(((myCollection.at(j))));
	}
	oo.print("\")");
/*
udanax-top.st:70522:WriteMemStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << (myIndex - myStart) << ', ' << (myMax - myIndex) << ', "'.
	myStart almostTo: myIndex do: [:i {Int32} | oo DOTput: ((myCollection at: i) basicCast: Character)].
	oo << '<-|->'.
	myIndex almostTo: myMax do: [:j  {Int32} | oo DOTput: ((myCollection at: j) basicCast: Character)].
	oo << '")'!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:70531:WriteMemStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:70533:WriteMemStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void initTimeNonInherited() {
	SomeStreams = InstanceCache.make(8);
/*
udanax-top.st:70544:WriteMemStream class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeStreams := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeStreams = null;
/*
udanax-top.st:70547:WriteMemStream class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeStreams := NULL!
*/
}
public static XnWriteStream make(UInt8Array dataP, int start, int count) {
	Heaper result;
	result = SomeStreams.fetch();
	if (result == null) {
		return new WriteMemStream(dataP, start, count);
	}
	else {
		return 
		/* TODO newBecome */
		new WriteMemStream(dataP, start, count);
	}
/*
udanax-top.st:70552:WriteMemStream class methodsFor: 'creation'!
{XnWriteStream} make: dataP {UInt8 star} with: start {Int32} with: count {Int32}
	| result {Heaper} |
	result := SomeStreams fetch.
	result == NULL
		ifTrue: [^WriteMemStream create: dataP with: start with: count]
		ifFalse: [^(WriteMemStream new.Become: result) create: dataP with: start with: count]!
*/
}
public WriteMemStream() {
/*

Generated during transformation
*/
}
public WriteMemStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
