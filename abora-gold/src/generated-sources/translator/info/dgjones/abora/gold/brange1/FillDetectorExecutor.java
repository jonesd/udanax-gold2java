/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.brange1;

import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.brange1.FillDetectorExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * This class notifies its place holder when its last fill detector has gone away.
 */
public class FillDetectorExecutor extends XnExecutor {

	protected BePlaceHolder myPlaceHolder;
/*
udanax-top.st:26447:
XnExecutor subclass: #FillDetectorExecutor
	instanceVariableNames: 'myPlaceHolder {BePlaceHolder}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-brange1'!
*/
/*
udanax-top.st:26451:
FillDetectorExecutor comment:
'This class notifies its place holder when its last fill detector has gone away.'!
*/
/*
udanax-top.st:26453:
(FillDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:26469:
FillDetectorExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26472:
(FillDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FillDetectorExecutor.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public FillDetectorExecutor(BePlaceHolder placeHolder) {
	super();
	myPlaceHolder = placeHolder;
/*
udanax-top.st:26458:FillDetectorExecutor methodsFor: 'protected: create'!
create: placeHolder {BePlaceHolder}
	super create.
	myPlaceHolder := placeHolder.!
*/
}
public void execute(int arg) {
	if (arg == 0) {
		myPlaceHolder.removeLastDetector();
	}
/*
udanax-top.st:26464:FillDetectorExecutor methodsFor: 'execute'!
{void} execute: arg {Int32}
	arg == Int32Zero ifTrue: [
		myPlaceHolder removeLastDetector]!
*/
}
public static XnExecutor make(BePlaceHolder placeHolder) {
	return new FillDetectorExecutor(placeHolder);
/*
udanax-top.st:26477:FillDetectorExecutor class methodsFor: 'create'!
{XnExecutor} make: placeHolder {BePlaceHolder}
	^ self create: placeHolder!
*/
}
public FillDetectorExecutor() {
/*

Generated during transformation
*/
}
public FillDetectorExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
