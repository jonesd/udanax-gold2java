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
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Binary2Rcvr;
import info.dgjones.abora.gold.xcvr.Binary2XcvrMaker;
import info.dgjones.abora.gold.xcvr.Binary2Xmtr;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;

public class Binary2XcvrMaker extends XcvrMaker {

/*
udanax-top.st:64115:
XcvrMaker subclass: #Binary2XcvrMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64119:
(Binary2XcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:64144:
Binary2XcvrMaker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:64147:
(Binary2XcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Binary2XcvrMaker.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public SpecialistRcvr makeRcvr(TransferSpecialist specialist, XnReadStream readStream) {
	return Binary2Rcvr.make(specialist, readStream);
/*
udanax-top.st:64124:Binary2XcvrMaker methodsFor: 'xcvr creation'!
{SpecialistRcvr} makeRcvr: specialist {TransferSpecialist} with: readStream {XnReadStream}
	^Binary2Rcvr make: specialist with: readStream!
*/
}
public SpecialistXmtr makeXmtr(TransferSpecialist specialist, XnWriteStream writeStream) {
	return Binary2Xmtr.make(specialist, writeStream);
/*
udanax-top.st:64127:Binary2XcvrMaker methodsFor: 'xcvr creation'!
{SpecialistXmtr} makeXmtr: specialist {TransferSpecialist} with: writeStream {XnWriteStream}
	^Binary2Xmtr make: specialist with: writeStream!
*/
}
public String id() {
	return "binary2";
/*
udanax-top.st:64132:Binary2XcvrMaker methodsFor: 'testing'!
{char star} id
	^'binary2'!
*/
}
public Binary2XcvrMaker(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:64137:Binary2XcvrMaker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:64140:Binary2XcvrMaker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static XcvrMaker make() {
	return new Binary2XcvrMaker();
/*
udanax-top.st:64152:Binary2XcvrMaker class methodsFor: 'creation'!
{XcvrMaker} make
	^self create!
*/
}
public Binary2XcvrMaker() {
/*

Generated during transformation
*/
}
}
