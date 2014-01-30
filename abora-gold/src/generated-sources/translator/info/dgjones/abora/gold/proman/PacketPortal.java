/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PacketPortal;
import info.dgjones.abora.gold.proman.Portal;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnBufferedReadStream;
import info.dgjones.abora.gold.xcvr.XnBufferedWriteStream;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;

public class PacketPortal extends Portal {

	protected XnReadStream myReadStream;
	protected XnWriteStream myWriteStream;
/*
udanax-top.st:31369:
Portal subclass: #PacketPortal
	instanceVariableNames: '
		myReadStream {XnReadStream}
		myWriteStream {XnWriteStream}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:31375:
(PacketPortal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PacketPortal.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public PacketPortal() {
	super();
	myReadStream = new XnBufferedReadStream(this);
	myWriteStream = new XnBufferedWriteStream(this);
/*
udanax-top.st:31380:PacketPortal methodsFor: 'protected: creation'!
create
	super create.
	myReadStream _ XnBufferedReadStream create: self.
	myWriteStream _ XnBufferedWriteStream create: self.!
*/
}
public PacketPortal(XnReadStream readStr, XnWriteStream writeStr) {
	super();
	myReadStream = readStr;
	myWriteStream = writeStr;
/*
udanax-top.st:31385:PacketPortal methodsFor: 'protected: creation'!
create: readStr {XnReadStream} with: writeStr {XnWriteStream}
	super create.
	myReadStream _ readStr.
	myWriteStream _ writeStr!
*/
}
public XnReadStream readStream() {
	return myReadStream;
/*
udanax-top.st:31392:PacketPortal methodsFor: 'accessing'!
{XnReadStream} readStream
	
	^myReadStream!
*/
}
public XnWriteStream writeStream() {
	return myWriteStream;
/*
udanax-top.st:31396:PacketPortal methodsFor: 'accessing'!
{XnWriteStream} writeStream
	
	^myWriteStream!
*/
}
/**
 * Make sure the bits go out.
 */
public void flush() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31402:PacketPortal methodsFor: 'internal'!
{void} flush
	"Make sure the bits go out."
	
	self subclassResponsibility!
*/
}
/**
 * Return a buffer of a size that the unerlying transport layer likes.
 */
public UInt8Array readBuffer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31407:PacketPortal methodsFor: 'internal'!
{UInt8Array} readBuffer
	"Return a buffer of a size that the unerlying transport layer likes."
	
	self subclassResponsibility!
*/
}
public int readPacket(UInt8Array buffer, int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31412:PacketPortal methodsFor: 'internal'!
{Int32} readPacket: buffer {UInt8Array} with: count {Int32}
	
	self subclassResponsibility!
*/
}
/**
 * Return a buffer of a size that the unerlying transport layer likes.
 */
public UInt8Array writeBuffer() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31416:PacketPortal methodsFor: 'internal'!
{UInt8Array} writeBuffer
	"Return a buffer of a size that the unerlying transport layer likes."
	
	self subclassResponsibility!
*/
}
public void writePacket(UInt8Array packet, int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:31421:PacketPortal methodsFor: 'internal'!
{void} writePacket: packet {UInt8Array} with: count {Int32}
	
	self subclassResponsibility!
*/
}
public PacketPortal(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
