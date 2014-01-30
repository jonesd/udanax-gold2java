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
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.urdi.HashStream;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HashStream extends XnWriteStream {

	protected int myHash;
/*
udanax-top.st:70349:
XnWriteStream subclass: #HashStream
	instanceVariableNames: 'myHash {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Urdi'!
*/
/*
udanax-top.st:70353:
(HashStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:70387:
HashStream class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:70390:
(HashStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HashStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public HashStream() {
	super();
	myHash = 0;
/*
udanax-top.st:70358:HashStream methodsFor: 'create'!
create
	super create.
	myHash := UInt32Zero!
*/
}
public void flush() {
/*
udanax-top.st:70364:HashStream methodsFor: 'accessing'!
{void} flush!
*/
}
/**
 * The accumulated hash
 */
public int hash() {
	return myHash;
/*
udanax-top.st:70366:HashStream methodsFor: 'accessing'!
{UInt32} hash
	"The accumulated hash"
	^myHash!
*/
}
public void putByte(int bytex) {
	myHash = FHash.fastHashUInt32((myHash ^ bytex));
/*
udanax-top.st:70370:HashStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32}
	myHash _ FHash fastHash.UInt32: (myHash bitXor: byte)!
*/
}
public void putData(UInt8Array array) {
	myHash = myHash ^ array.contentsHash();
/*
udanax-top.st:70373:HashStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	myHash := myHash bitXor: array contentsHash!
*/
}
public void putStr(String string) {
	myHash = myHash ^ (FHash.fastHashString(string));
/*
udanax-top.st:70377:HashStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	myHash _ myHash bitXor: (FHash fastHash.String: string)!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:70382:HashStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:70384:HashStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static XnWriteStream make() {
	return new HashStream();
/*
udanax-top.st:70395:HashStream class methodsFor: 'creation'!
{XnWriteStream} make
	^self create!
*/
}
public HashStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
