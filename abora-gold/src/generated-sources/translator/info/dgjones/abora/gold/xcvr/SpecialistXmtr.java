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
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.xcvr.CommIbid;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * myIbids maps from already sent heapers to their ibid numbers.
 */
public class SpecialistXmtr extends Xmtr {

	protected TransferSpecialist mySpecialist;
	protected PrimIndexTable myIbids;
	protected int myNextIbid;
	protected static PrimIndexTable XmtrIbidCache;
/*
udanax-top.st:64297:
Xmtr subclass: #SpecialistXmtr
	instanceVariableNames: '
		mySpecialist {TransferSpecialist}
		myIbids {PrimIndexTable}
		myNextIbid {Int4}'
	classVariableNames: 'XmtrIbidCache {PrimIndexTable} '
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64304:
SpecialistXmtr comment:
'myIbids maps from already sent heapers to their ibid numbers.'!
*/
/*
udanax-top.st:64306:
(SpecialistXmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:64424:
SpecialistXmtr class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64427:
(SpecialistXmtr getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SpecialistXmtr.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public void sendBooleanVar(boolean b) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64311:SpecialistXmtr methodsFor: 'sending'!
{void} sendBooleanVar: b {BooleanVar}
	self subclassResponsibility!
*/
}
public void sendCategory(Category cat) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64314:SpecialistXmtr methodsFor: 'sending'!
{void} sendCategory: cat {Category}
	self subclassResponsibility!
*/
}
public void sendHeaper(Heaper object) {
	if (object == null) {
		sendNULL();
	}
	else {
		int pos;
		pos = (myIbids.fetch(object));
		if (pos != -1) {
			sendIbid(pos);
		}
		else {
			mySpecialist.sendHeaperTo(object, this);
		}
	}
/*
udanax-top.st:64317:SpecialistXmtr methodsFor: 'sending'!
{void} sendHeaper: object {Heaper} 
	object == NULL
		ifTrue: [self sendNULL]
		ifFalse: 
			[| pos {Int32} |
			pos _ (myIbids fetch: object) DOTasLong.
			pos ~~ -1
				ifTrue: [self sendIbid: pos]
				ifFalse: [mySpecialist sendHeaper: object to: self]]!
*/
}
public void sendIEEEDoubleVar(double x) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64327:SpecialistXmtr methodsFor: 'sending'!
{void} sendIEEEDoubleVar: x {IEEEDoubleVar}
	
	self subclassResponsibility!
*/
}
public void sendInt32(int n) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64331:SpecialistXmtr methodsFor: 'sending'!
{void} sendInt32: n {Int32}
	
	self subclassResponsibility!
*/
}
public void sendInt8(int bytex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64335:SpecialistXmtr methodsFor: 'sending'!
{void} sendInt8: byte {Int8}
	
	self subclassResponsibility!
*/
}
public void sendIntegerVar(int n) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64339:SpecialistXmtr methodsFor: 'sending'!
{void} sendIntegerVar: n {IntegerVar}
	
	self subclassResponsibility!
*/
}
public void sendString(String s) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64343:SpecialistXmtr methodsFor: 'sending'!
{void} sendString: s {char star}
	self subclassResponsibility!
*/
}
public void sendUInt32(int n) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64347:SpecialistXmtr methodsFor: 'sending'!
{void} sendUInt32: n {UInt32}
	
	self subclassResponsibility!
*/
}
public void sendUInt8(int bytex) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64351:SpecialistXmtr methodsFor: 'sending'!
{void} sendUInt8: byte {UInt8}
	
	self subclassResponsibility!
*/
}
/**
 * Send the contents of the UInt8Array as data.
 */
public void sendUInt8Data(UInt8Array array) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64355:SpecialistXmtr methodsFor: 'sending'!
{void} sendUInt8Data: array {UInt8Array}
	"Send the contents of the UInt8Array as data."
	self subclassResponsibility!
*/
}
public void endInstance() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64362:SpecialistXmtr methodsFor: 'specialist sending'!
{void} endInstance
	self subclassResponsibility!
*/
}
/**
 * Register heaper as an object to be sent across the wire, and send cat
 * as its category. cat might be different from heaper->getCategory()
 * if another object is being substituted for heaper.
 */
public void startInstance(Heaper heaper, Category cat) {
	myIbids.introduce(heaper, myNextIbid);
	myNextIbid = myNextIbid + 1;
	startNewInstance(cat);
/*
udanax-top.st:64366:SpecialistXmtr methodsFor: 'specialist sending'!
{void} startInstance: heaper {Heaper} with: cat {Category} 
	"Register heaper as an object to be sent across the wire, and send cat
	 as its category. cat might be different from heaper->getCategory() 
	 if another object is being substituted for heaper."
	
	myIbids at: heaper introduce: myNextIbid.
	myNextIbid _ myNextIbid + 1.
	self startNewInstance: cat!
*/
}
public void endPacket() {
	myIbids.clearAll();
	myNextIbid = 0;
/*
udanax-top.st:64377:SpecialistXmtr methodsFor: 'protected:'!
{void} endPacket
	myIbids clearAll.
	myNextIbid _ Int32Zero!
*/
}
public void sendNULL() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64381:SpecialistXmtr methodsFor: 'protected:'!
{void} sendNULL
	self subclassResponsibility!
*/
}
public TransferSpecialist specialist() {
	return mySpecialist;
/*
udanax-top.st:64385:SpecialistXmtr methodsFor: 'protected:'!
{TransferSpecialist} specialist
	^mySpecialist!
*/
}
public void startNewInstance(Category cat) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64388:SpecialistXmtr methodsFor: 'protected:'!
{void} startNewInstance: cat {Category} 
	
	self subclassResponsibility!
*/
}
/**
 * The object represented by pos has already been sent.  Send just a reference by number.
 */
public void sendIbid(int pos) {
	startNewInstance(AboraSupport.findCategory(CommIbid.class));
	sendInt32(pos);
	endInstance();
/*
udanax-top.st:64394:SpecialistXmtr methodsFor: 'private: sending'!
{void} sendIbid: pos {Int32}
	"The object represented by pos has already been sent.  Send just a reference by number."
	
	self startNewInstance: CommIbid.
	self sendInt32: pos.
	self endInstance!
*/
}
public SpecialistXmtr(TransferSpecialist specialist) {
	super();
	mySpecialist = specialist;
	if (XmtrIbidCache == null) {
		myIbids = PrimIndexTable.make(255);
	}
	else {
		myIbids = XmtrIbidCache;
		XmtrIbidCache = null;
	}
	myNextIbid = 0;
/*
udanax-top.st:64403:SpecialistXmtr methodsFor: 'protected: creation'!
create: specialist {TransferSpecialist}
	super create.
	mySpecialist _ specialist.
	XmtrIbidCache == NULL 
		ifTrue: [myIbids _ PrimIndexTable make: 255]
		ifFalse:
			[myIbids _ XmtrIbidCache.
			XmtrIbidCache _ NULL].
	myNextIbid _ Int32Zero!
*/
}
public void destruct() {
	if (XmtrIbidCache == null) {
		myIbids.clearAll();
		XmtrIbidCache = myIbids;
		myIbids = null;
	}
	else {
		myIbids.destroy();
	}
	/* mySpecialist destroy */
	super.destruct();
/*
udanax-top.st:64413:SpecialistXmtr methodsFor: 'protected: creation'!
{void} destruct
	XmtrIbidCache == NULL 
		ifTrue:
			[myIbids clearAll.
			XmtrIbidCache _ myIbids cast: PrimIndexTable.
			myIbids _ NULL]
		ifFalse: [myIbids destroy].
	"mySpecialist destroy"
	super destruct!
*/
}
public static void linkTimeNonInherited() {
	XmtrIbidCache = null;
/*
udanax-top.st:64432:SpecialistXmtr class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	XmtrIbidCache _ NULL!
*/
}
public SpecialistXmtr() {
/*

Generated during transformation
*/
}
public SpecialistXmtr(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
