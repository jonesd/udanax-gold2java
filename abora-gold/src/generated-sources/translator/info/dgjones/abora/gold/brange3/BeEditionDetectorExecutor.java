/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.brange3;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.brange3.BeEditionDetectorExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * This class notifies its edition when its last detector has gone.
 */
public class BeEditionDetectorExecutor extends XnExecutor {

	protected BeEdition myEdition;
/*
udanax-top.st:12877:
XnExecutor subclass: #BeEditionDetectorExecutor
	instanceVariableNames: 'myEdition {BeEdition}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-brange3'!
*/
/*
udanax-top.st:12881:
BeEditionDetectorExecutor comment:
'This class notifies its edition when its last detector has gone.'!
*/
/*
udanax-top.st:12883:
(BeEditionDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:12899:
BeEditionDetectorExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12902:
(BeEditionDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeEditionDetectorExecutor.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public BeEditionDetectorExecutor(BeEdition edition) {
	super();
	myEdition = edition;
/*
udanax-top.st:12888:BeEditionDetectorExecutor methodsFor: 'protected: create'!
create: edition {BeEdition}
	super create.
	myEdition := edition.!
*/
}
public void execute(int arg) {
	if (arg == 0) {
		myEdition.removeLastDetector();
	}
/*
udanax-top.st:12894:BeEditionDetectorExecutor methodsFor: 'execute'!
{void} execute: arg {Int32}
	arg == Int32Zero ifTrue: [
		myEdition removeLastDetector].!
*/
}
public static XnExecutor make(BeEdition edition) {
	return new BeEditionDetectorExecutor(edition);
/*
udanax-top.st:12907:BeEditionDetectorExecutor class methodsFor: 'creation'!
{XnExecutor} make: edition {BeEdition}
	^ self create: edition!
*/
}
public BeEditionDetectorExecutor() {
/*

Generated during transformation
*/
}
public BeEditionDetectorExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
