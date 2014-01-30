/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PairPortal;
import info.dgjones.abora.gold.proman.Portal;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;

public class PairPortal extends Portal {

	protected XnReadStream myReadStream;
	protected XnWriteStream myWriteStream;
/*
udanax-top.st:31425:
Portal subclass: #PairPortal
	instanceVariableNames: '
		myReadStream {XnReadStream}
		myWriteStream {XnWriteStream}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:31431:
(PairPortal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:31457:
PairPortal class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:31460:
(PairPortal getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PairPortal.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public XnReadStream readStream() {
	return myReadStream;
/*
udanax-top.st:31436:PairPortal methodsFor: 'accessing'!
{XnReadStream} readStream
	
	^myReadStream!
*/
}
public XnWriteStream writeStream() {
	return myWriteStream;
/*
udanax-top.st:31440:PairPortal methodsFor: 'accessing'!
{XnWriteStream} writeStream
	
	^myWriteStream!
*/
}
public PairPortal(XnReadStream readStr, XnWriteStream writeStr) {
	super();
	myReadStream = readStr;
	myWriteStream = writeStr;
/*
udanax-top.st:31446:PairPortal methodsFor: 'protected: creation'!
create: readStr {XnReadStream} with: writeStr {XnWriteStream}
	super create.
	myReadStream _ readStr.
	myWriteStream _ writeStr!
*/
}
public void destruct() {
	myReadStream.destroy();
	myWriteStream.destroy();
	super.destruct();
/*
udanax-top.st:31451:PairPortal methodsFor: 'protected: creation'!
{void} destruct
	myReadStream destroy.
	myWriteStream destroy.
	super destruct!
*/
}
public static PairPortal make(XnReadStream read, XnWriteStream write) {
	return new PairPortal(read, write);
/*
udanax-top.st:31465:PairPortal class methodsFor: 'creation'!
make: read {XnReadStream} with: write {XnWriteStream}
	^self create: read with: write!
*/
}
public PairPortal() {
/*

Generated during transformation
*/
}
public PairPortal(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
