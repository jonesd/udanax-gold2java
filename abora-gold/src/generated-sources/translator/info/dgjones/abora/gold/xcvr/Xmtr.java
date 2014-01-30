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
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Xmtr extends Heaper {

/*
udanax-top.st:64227:
Heaper subclass: #Xmtr
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64231:
(Xmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Xmtr.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public void sendBooleanVar(boolean b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64236:Xmtr methodsFor: 'sending'!
{void} sendBooleanVar: b {BooleanVar}
	self subclassResponsibility!
*/
}
public void sendHeaper(Heaper object) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64239:Xmtr methodsFor: 'sending'!
{void} sendHeaper: object {Heaper}
	
	self subclassResponsibility!
*/
}
public void sendIEEEDoubleVar(double x) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64243:Xmtr methodsFor: 'sending'!
{void} sendIEEEDoubleVar: x {IEEEDoubleVar}
	
	self subclassResponsibility!
*/
}
public void sendInt32(int n) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64247:Xmtr methodsFor: 'sending'!
{void} sendInt32: n {Int32}
	
	self subclassResponsibility!
*/
}
public void sendInt8(int bytex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64251:Xmtr methodsFor: 'sending'!
{void} sendInt8: byte {Int8}
	
	self subclassResponsibility!
*/
}
public void sendIntegerVar(int n) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64255:Xmtr methodsFor: 'sending'!
{void} sendIntegerVar: n {IntegerVar}
	
	self subclassResponsibility!
*/
}
public void sendString(String s) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64259:Xmtr methodsFor: 'sending'!
{void} sendString: s {char star}
	self subclassResponsibility!
*/
}
public void sendUInt32(int n) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64263:Xmtr methodsFor: 'sending'!
{void} sendUInt32: n {UInt32}
	
	self subclassResponsibility!
*/
}
public void sendUInt8(int bytex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64267:Xmtr methodsFor: 'sending'!
{void} sendUInt8: byte {UInt8}
	
	self subclassResponsibility!
*/
}
public void sendUInt8Data(UInt8Array array) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64271:Xmtr methodsFor: 'sending'!
{void} sendUInt8Data: array {UInt8Array}
	self subclassResponsibility!
*/
}
/*
udanax-top.st:64277:Xmtr methodsFor: 'smalltalk: sending'!
{void} send: object {Object}
	"Dispatch to the send routines."
	(object isInteger) ifTrue: [self sendIntegerVar: object]
	ifFalse: [object == true ifTrue: [self sendUInt32: 1]
	ifFalse: [object == false ifTrue: [self sendUInt32: UInt32Zero]
	ifFalse: [self sendHeaper: object]]]!
*/
public void sendData(UInt8Array array) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64287:Xmtr methodsFor: 'smalltalk: deja vu'!
{void} sendData: array {UInt8Array}
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:64293:Xmtr methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:64295:Xmtr methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public Xmtr() {
/*

Generated during transformation
*/
}
public Xmtr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
