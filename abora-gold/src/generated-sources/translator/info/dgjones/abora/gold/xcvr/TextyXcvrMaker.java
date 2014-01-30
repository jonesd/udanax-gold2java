/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TextyRcvr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.TextyXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;

public class TextyXcvrMaker extends XcvrMaker {

/*
udanax-top.st:64179:
XcvrMaker subclass: #TextyXcvrMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64183:
(TextyXcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:64210:
TextyXcvrMaker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64213:
(TextyXcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TextyXcvrMaker.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public SpecialistRcvr makeRcvr(TransferSpecialist specialist, XnReadStream readStream) {
	return new TextyRcvr(specialist, readStream);
/*
udanax-top.st:64188:TextyXcvrMaker methodsFor: 'xcvr creation'!
{SpecialistRcvr} makeRcvr: specialist {TransferSpecialist} with: readStream {XnReadStream}
	^TextyRcvr create: specialist with: readStream!
*/
}
public SpecialistXmtr makeXmtr(TransferSpecialist specialist, XnWriteStream writeStream) {
	return new TextyXmtr(specialist, writeStream);
/*
udanax-top.st:64192:TextyXcvrMaker methodsFor: 'xcvr creation'!
{SpecialistXmtr} makeXmtr: specialist {TransferSpecialist} with: writeStream {XnWriteStream}
	^TextyXmtr create: specialist with: writeStream!
*/
}
public String id() {
	return "texty";
/*
udanax-top.st:64198:TextyXcvrMaker methodsFor: 'testing'!
{char star} id
	^'texty'!
*/
}
public TextyXcvrMaker(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:64203:TextyXcvrMaker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:64206:TextyXcvrMaker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static XcvrMaker make() {
	return new TextyXcvrMaker();
/*
udanax-top.st:64218:TextyXcvrMaker class methodsFor: 'creation'!
{XcvrMaker} make
	^self create!
*/
}
public static Rcvr makeReader(XnReadStream stream) {
	return new TextyRcvr((TransferSpecialist.make((Cookbook.make()))), stream);
/*
udanax-top.st:64221:TextyXcvrMaker class methodsFor: 'creation'!
{Rcvr} makeReader: stream {XnReadStream}
	^TextyRcvr create: (TransferSpecialist make: (Cookbook make)) with: stream.!
*/
}
public static Xmtr makeWriter(XnWriteStream stream) {
	return new TextyXmtr((TransferSpecialist.make((Cookbook.make()))), stream);
/*
udanax-top.st:64224:TextyXcvrMaker class methodsFor: 'creation'!
{Xmtr} makeWriter: stream {XnWriteStream}
	^TextyXmtr create: (TransferSpecialist make: (Cookbook make)) with: stream.!
*/
}
public TextyXcvrMaker() {
/*

Generated during transformation
*/
}
}
