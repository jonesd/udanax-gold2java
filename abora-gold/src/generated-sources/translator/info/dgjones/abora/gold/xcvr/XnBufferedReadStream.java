/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PacketPortal;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnBufferedReadStream;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class XnBufferedReadStream extends XnReadStream {

	protected PacketPortal myPortal;
	protected UInt8Array myBuffer;
	protected int myNext;
	protected int myMax;
/*
udanax-top.st:65069:
XnReadStream subclass: #XnBufferedReadStream
	instanceVariableNames: '
		myPortal {PacketPortal}
		myBuffer {UInt8Array}
		myNext {Int32}
		myMax {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:65077:
(XnBufferedReadStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XnBufferedReadStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public UInt8Array contents() {
	if (myBuffer == null) {
		return (UInt8Array) (PrimIntArray.zeros(8, 0));
	}
	return (UInt8Array) (myBuffer.copy(myMax - myNext, myNext));
/*
udanax-top.st:65082:XnBufferedReadStream methodsFor: 'accessing'!
{UInt8Array} contents
	
	myBuffer == NULL ifTrue: [^(PrimIntArray zeros: 8 with: Int32Zero) cast: UInt8Array].
	^(myBuffer copy: myMax - myNext with: myNext) cast: UInt8Array!
*/
}
public int getByte() {
	int result;
	while (myNext >= myMax) {
		refill();
	}
	result = myBuffer.uIntAt(myNext);
	myNext = myNext + 1;
	return result;
/*
udanax-top.st:65087:XnBufferedReadStream methodsFor: 'accessing'!
{UInt8} getByte
	
	| result {UInt8} |
	[myNext >= myMax] whileTrue: [self refill].
	result _ myBuffer uIntAt: myNext.
	myNext _ myNext + 1.
	^result!
*/
}
/**
 * Pour data directly into a buffer.
 */
public void getBytes(UInt8Array buffer, int count, int start) {
	Someone.thingToDo();
	for (int i = 
	/* Make a more efficient version of this. */
	start; i < start + count; i ++ ) {
		(buffer).put(i, getByte());
	}
/*
udanax-top.st:65095:XnBufferedReadStream methodsFor: 'accessing'!
{void} getBytes: buffer {void star} with: count {Int32} with: start {Int32 default: Int32Zero}
	"Pour data directly into a buffer."
	
	self thingToDo.  "Make a more efficient version of this."
	start almostTo: start + count do: [:i {Int32} |
		(buffer basicCast: Character star) at: i put: self getByte]!
*/
}
public boolean isReady() {
	refill();
	return myNext < myMax;
/*
udanax-top.st:65102:XnBufferedReadStream methodsFor: 'accessing'!
{BooleanVar} isReady
	
	self refill.
	^myNext < myMax!
*/
}
public void putBack(int c) {
	if (myNext <= 0) {
		throw new AboraRuntimeException(AboraRuntimeException.BEGINNING_OF_PACKET);
	}
	if ((myBuffer.uIntAt(myNext-1)) != c) {
		throw new AboraRuntimeException(AboraRuntimeException.DOES_NOT_MATCH);
	}
	myNext = myNext-1;
/*
udanax-top.st:65107:XnBufferedReadStream methodsFor: 'accessing'!
{void} putBack: c {UInt8}
	
	(myNext <= Int32Zero)
		ifTrue: [Heaper BLAST: #BeginningOfPacket].
	(myBuffer uIntAt: myNext -1) ~= c
		ifTrue: [Heaper BLAST: #DoesNotMatch].
	myNext _ myNext -1.!
*/
}
public void refill() {
	if ( ! (myNext < myMax)) {
		if (myBuffer == null) {
			myBuffer = myPortal.readBuffer();
		}
		myMax = myPortal.readPacket(myBuffer, myBuffer.count());
		myNext = 0;
	}
/*
udanax-top.st:65115:XnBufferedReadStream methodsFor: 'accessing'!
{void} refill
	
	(myNext < myMax) ifFalse:
		[myBuffer == NULL ifTrue: [myBuffer _ myPortal readBuffer].
		myMax _ myPortal readPacket: myBuffer with: myBuffer count.
		myNext _ Int32Zero]!
*/
}
public XnBufferedReadStream(PacketPortal portal) {
	super();
	myPortal = portal;
	myBuffer = null;
	myNext = 0;
	myMax = myNext;
/*
udanax-top.st:65124:XnBufferedReadStream methodsFor: 'creation'!
create: portal {PacketPortal}
	super create.
	myPortal _ portal.
	myBuffer _ NULL.
	myNext _ Int32Zero.
	myMax _ myNext!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myNext);
	oo.print(", ");
	oo.print(myMax);
	if (myBuffer == null) {
		oo.print(")");
		return ;
	}
	oo.print(", \"");
	for (int i = 0; i < myNext; i ++ ) {
		oo.print(((myBuffer.uIntAt(i))));
	}
	oo.print("<-|->");
	for (int j = myNext; j < myMax; j ++ ) {
		oo.print(((myBuffer.uIntAt(j))));
	}
	oo.print("\")");
/*
udanax-top.st:65134:XnBufferedReadStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	
	oo << self getCategory name << '(' << myNext << ', ' << myMax.
	myBuffer == NULL ifTrue: [oo << ')'.  ^VOID].
	oo << ', "'.
	Int32Zero almostTo: myNext do: [:i {Int32} | oo DOTput: ((myBuffer uIntAt: i) basicCast: Character)].
	oo << '<-|->'.
	myNext almostTo: myMax do: [:j {Int32} | oo DOTput: ((myBuffer uIntAt: j) basicCast: Character)].
	oo << '")'!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:65146:XnBufferedReadStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:65148:XnBufferedReadStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public XnBufferedReadStream() {
/*

Generated during transformation
*/
}
public XnBufferedReadStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
