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
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Rcvr extends Heaper {

/*
udanax-top.st:41017:
Heaper subclass: #Rcvr
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:41021:
(Rcvr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Rcvr.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public boolean receiveBooleanVar() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41026:Rcvr methodsFor: 'receiving'!
{BooleanVar} receiveBooleanVar
	self subclassResponsibility!
*/
}
/**
 * Fill the array with data from the stream.
 */
public void receiveData(UInt8Array array) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41029:Rcvr methodsFor: 'receiving'!
{void} receiveData: array {UInt8Array}
	"Fill the array with data from the stream."
	
	self subclassResponsibility!
*/
}
public Heaper receiveHeaper() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41034:Rcvr methodsFor: 'receiving'!
{Heaper} receiveHeaper
	self subclassResponsibility!
*/
}
public double receiveIEEEDoubleVar() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41037:Rcvr methodsFor: 'receiving'!
{IEEEDoubleVar} receiveIEEEDoubleVar
	self subclassResponsibility!
*/
}
public int receiveInt32() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41040:Rcvr methodsFor: 'receiving'!
{Int32} receiveInt32
	self subclassResponsibility!
*/
}
public int receiveInt8() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41043:Rcvr methodsFor: 'receiving'!
{Int8} receiveInt8
	self subclassResponsibility!
*/
}
public int receiveIntegerVar() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41046:Rcvr methodsFor: 'receiving'!
{IntegerVar} receiveIntegerVar
	self subclassResponsibility!
*/
}
/**
 * Receive an object into another object.
 */
public void receiveInto(Heaper memory) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41049:Rcvr methodsFor: 'receiving'!
{void} receiveInto: memory {Heaper}
	"Receive an object into another object."
	self subclassResponsibility!
*/
}
public String receiveString() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41054:Rcvr methodsFor: 'receiving'!
{char star} receiveString
	self subclassResponsibility!
*/
}
public int receiveUInt32() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41058:Rcvr methodsFor: 'receiving'!
{UInt32} receiveUInt32
	self subclassResponsibility!
*/
}
public int receiveUInt8() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:41061:Rcvr methodsFor: 'receiving'!
{UInt8} receiveUInt8
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:41066:Rcvr methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:41068:Rcvr methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public Rcvr() {
/*

Generated during transformation
*/
}
public Rcvr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
