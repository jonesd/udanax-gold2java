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
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.ReadMemStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;
import java.io.StringWriter;

public class ReadMemStream extends XnReadStream {

	protected UInt8Array myBuffer;
	protected int myIndex;
	protected int myStart;
	protected int myEnd;
	protected static InstanceCache SomeStreams;
/*
udanax-top.st:64971:
XnReadStream subclass: #ReadMemStream
	instanceVariableNames: '
		myBuffer {UInt8 star}
		myIndex {Int32}
		myStart {Int32}
		myEnd {Int32}'
	classVariableNames: 'SomeStreams {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:64979:
(ReadMemStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:65046:
ReadMemStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:65049:
(ReadMemStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ReadMemStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int getByte() {
	int result;
	if ( ! (myIndex < myEnd)) {
		throw new AboraAssertionException("Stay within stream");
	}
	/* Removed smalltalkOnly */
	result = myBuffer.at(myIndex);
	myIndex = myIndex + 1;
	return result;
/*
udanax-top.st:64984:ReadMemStream methodsFor: 'accessing'!
{UInt8} getByte
	| result {UInt8} |
	myIndex < myEnd assert: 'Stay within stream'.
	[result _ myBuffer uIntAt: myIndex] smalltalkOnly.
	[result _ myBuffer at: myIndex] translateOnly.
	myIndex _ myIndex + 1.
	^result!
*/
}
public void putBack(int b) {
	if ( ! (myIndex > myStart)) {
		throw new AboraAssertionException("Must have room");
	}
	myIndex = myIndex - 1;
	/* Removed smalltalkOnly */
	if ( ! ((myBuffer.at(myIndex)) == b)) {
		throw new AboraAssertionException("Must be same character");
	}
/*
udanax-top.st:64992:ReadMemStream methodsFor: 'accessing'!
{void} putBack: b {UInt8}
	myIndex > myStart assert: 'Must have room'.
	myIndex _ myIndex - 1.
	[(myBuffer uIntAt: myIndex) == b assert: 'Must be same character'.] smalltalkOnly.
	[(myBuffer at: myIndex) == b assert: 'Must be same character'] translateOnly!
*/
}
public void refill() {
/*
udanax-top.st:64998:ReadMemStream methodsFor: 'accessing'!
{void} refill!
*/
}
public ReadMemStream(UInt8Array collection, int index, int count) {
	super();
	myBuffer = collection;
	myIndex = index;
	myStart = index;
	myEnd = index + count;
/*
udanax-top.st:65002:ReadMemStream methodsFor: 'creation'!
create: collection {UInt8 star} with: index {Int32} with: count {Int32}
	super create.
	myBuffer _ collection.
	myIndex _ index.
	myStart _ index.
	myEnd _ index + count!
*/
}
public void destroy() {
	if ( ! (SomeStreams.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:65009:ReadMemStream methodsFor: 'creation'!
{void} destroy
	(SomeStreams store: self) ifFalse: [super destroy]!
*/
}
public boolean end() {
	return myIndex >= myEnd;
/*
udanax-top.st:65014:ReadMemStream methodsFor: 'smalltalk: streams'!
{BooleanVar} atEnd
	^myIndex >= myEnd!
*/
}
public int next() {
	int result;
	result = myBuffer.at(myIndex + 1);
	myIndex = myIndex + 1;
	/* Removed smalltalkOnly */
	return result;
/*
udanax-top.st:65017:ReadMemStream methodsFor: 'smalltalk: streams'!
{UInt8} next
	| result {UInt8} |
	result _ myBuffer at: myIndex+1.
	myIndex _ myIndex + 1.
	[^Character char: result] smalltalkOnly.
	[^result] translateOnly.!
*/
}
public String peekAhead() {
	StringWriter stringWriter = new StringWriter();
	PrintWriter strm = new PrintWriter(stringWriter);
	for (int i = 0; i <= (Math.min(myEnd - myIndex, 100)); i ++ ) {
		strm.print(((char) ((myBuffer.uIntAt(myIndex + i)))));
	}
	return stringWriter.toString();
/*
udanax-top.st:65024:ReadMemStream methodsFor: 'smalltalk: streams'!
{String} peekAhead
	^String streamContents: [:strm |
		0 to: (myEnd - myIndex min: 100)
		   do: [:i | strm nextPut: (Character char: (myBuffer uIntAt: myIndex + i))]]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print((myIndex - myStart));
	oo.print(", ");
	oo.print((myEnd - myIndex));
	oo.print(", \"");
	for (int i = myStart; i < myIndex; i ++ ) {
		oo.print(((myBuffer.at(i))));
	}
	oo.print("<-|->");
	for (int j = myIndex; j < myEnd; j ++ ) {
		oo.print(((myBuffer.at(j))));
	}
	oo.print("\")");
/*
udanax-top.st:65031:ReadMemStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	
	oo << self getCategory name << '(' << (myIndex - myStart) << ', ' << (myEnd - myIndex) << ', "'.
	myStart almostTo: myIndex do: [:i {Int32} | oo DOTput: ((myBuffer at: i) basicCast: Character)].
	oo << '<-|->'.
	myIndex almostTo: myEnd do: [:j {Int32} | oo DOTput: ((myBuffer at: j) basicCast: Character)].
	oo << '")'!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:65041:ReadMemStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:65043:ReadMemStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static XnReadStream make(UInt8Array dataP, int start, int count) {
	Heaper result;
	result = SomeStreams.fetch();
	if (result == null) {
		return new ReadMemStream(dataP, start, count);
	}
	else {
		return 
		/* TODO newBecome */
		new ReadMemStream(dataP, start, count);
	}
/*
udanax-top.st:65054:ReadMemStream class methodsFor: 'creation'!
{XnReadStream} make: dataP {UInt8 star} with: start {Int32} with: count {Int32}
	| result {Heaper} |
	result := SomeStreams fetch.
	result == NULL
		ifTrue: [^self create: dataP with: start with: count]
		ifFalse: [^(self new.Become: result) create: dataP with: start with: count]!
*/
}
public static void initTimeNonInherited() {
	SomeStreams = InstanceCache.make(8);
/*
udanax-top.st:65063:ReadMemStream class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeStreams := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeStreams = null;
/*
udanax-top.st:65066:ReadMemStream class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeStreams := NULL!
*/
}
public ReadMemStream() {
/*

Generated during transformation
*/
}
public ReadMemStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
