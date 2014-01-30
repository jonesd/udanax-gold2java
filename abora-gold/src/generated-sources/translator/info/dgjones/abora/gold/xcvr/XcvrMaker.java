/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.FHash;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.xcvr.BogusXcvrMaker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class XcvrMaker extends Heaper {

/*
udanax-top.st:64057:
Heaper subclass: #XcvrMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64061:
(XcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:64094:
XcvrMaker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64097:
(XcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XcvrMaker.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public SpecialistRcvr makeRcvr(TransferSpecialist specialist, XnReadStream readStream) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64066:XcvrMaker methodsFor: 'xcvr creation'!
{SpecialistRcvr} makeRcvr: specialist {TransferSpecialist} with: readStream {XnReadStream}
	self subclassResponsibility!
*/
}
public SpecialistXmtr makeXmtr(TransferSpecialist specialist, XnWriteStream writeStream) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64070:XcvrMaker methodsFor: 'xcvr creation'!
{SpecialistXmtr} makeXmtr: specialist {TransferSpecialist} with: writeStream {XnWriteStream}
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("A ");
	oo.print(getAboraClass().name());
/*
udanax-top.st:64076:XcvrMaker methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'A ' << self getCategory name!
*/
}
public int actualHashForEqual() {
	return getCategory().hashForEqual() * 997 + (FHash.fastHashString(id()));
/*
udanax-top.st:64081:XcvrMaker methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^self getCategory hashForEqual * 997 + (FHash fastHash.String: self id)!
*/
}
/**
 * Return the name by which this protocol should be known.
 */
public String id() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:64084:XcvrMaker methodsFor: 'testing'!
{char star} id
	"Return the name by which this protocol should be known."
	
	self subclassResponsibility!
*/
}
public boolean isEqual(Heaper other) {
	if (other instanceof XcvrMaker) {
		XcvrMaker xm = (XcvrMaker) other;
		return (id().compareTo(xm.id())) == 0;
	}
	else {
		return false;
	}
/*
udanax-top.st:64089:XcvrMaker methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	other cast: XcvrMaker into: [:xm | ^(String strcmp: self id with: xm id) == Int32Zero] others: [^false].
	^false "fodder"!
*/
}
public static XcvrMaker make() {
	return new BogusXcvrMaker();
/*
udanax-top.st:64102:XcvrMaker class methodsFor: 'xcvr creation'!
{XcvrMaker} make
	^BogusXcvrMaker create!
*/
}
public static void initTimeInherited() {
	XcvrMaker maker;
	maker = new XcvrMaker();
	ProtocolBroker.registerXcvrProtocol(maker);
/*
udanax-top.st:64107:XcvrMaker class methodsFor: 'smalltalk: initialization'!
initTimeInherited
	| maker {XcvrMaker} |
	self REQUIRES: ProtocolBroker.
	maker _ self create.
	ProtocolBroker registerXcvrProtocol: maker!
*/
}
public static void suppressInitTimeInherited() {
/*
udanax-top.st:64113:XcvrMaker class methodsFor: 'smalltalk: initialization'!
suppressInitTimeInherited!
*/
}
public XcvrMaker() {
/*

Generated during transformation
*/
}
public XcvrMaker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
