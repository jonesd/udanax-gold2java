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
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.BogusXcvrMaker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;

public class BogusXcvrMaker extends XcvrMaker {

/*
udanax-top.st:64155:
XcvrMaker subclass: #BogusXcvrMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:64159:
(BogusXcvrMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BogusXcvrMaker.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public String id() {
	return "bogus";
/*
udanax-top.st:64164:BogusXcvrMaker methodsFor: 'testing'!
{char star} id
	^'bogus'!
*/
}
public SpecialistRcvr makeRcvr(TransferSpecialist specialist, XnReadStream readStream) {
	throw new AboraRuntimeException(AboraRuntimeException.BOGUS_PROTOCOL);
/*
udanax-top.st:64169:BogusXcvrMaker methodsFor: 'xcvr creation'!
{SpecialistRcvr} makeRcvr: specialist {TransferSpecialist unused} with: readStream {XnReadStream unused}
	Heaper BLAST: #BogusProtocol.
	^NULL!
*/
}
public SpecialistXmtr makeXmtr(TransferSpecialist specialist, XnWriteStream writeStream) {
	throw new AboraRuntimeException(AboraRuntimeException.BOGUS_PROTOCOL);
/*
udanax-top.st:64174:BogusXcvrMaker methodsFor: 'xcvr creation'!
{SpecialistXmtr} makeXmtr: specialist {TransferSpecialist} with: writeStream {XnWriteStream}
	Heaper BLAST: #BogusProtocol.
	^NULL!
*/
}
public BogusXcvrMaker() {
/*

Generated during transformation
*/
}
public BogusXcvrMaker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
