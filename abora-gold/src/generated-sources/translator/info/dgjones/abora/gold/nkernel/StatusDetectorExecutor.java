/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nkernel.StatusDetectorExecutor;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * This class informs its work when its last status detector has gone away.
 */
public class StatusDetectorExecutor extends XnExecutor {

	protected FeWork myWork;
/*
udanax-top.st:52725:
XnExecutor subclass: #StatusDetectorExecutor
	instanceVariableNames: 'myWork {FeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:52729:
StatusDetectorExecutor comment:
'This class informs its work when its last status detector has gone away.'!
*/
/*
udanax-top.st:52731:
(StatusDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:52747:
StatusDetectorExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:52750:
(StatusDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(StatusDetectorExecutor.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute(int arg) {
	if (arg == 0) {
		myWork.removeLastStatusDetector();
	}
/*
udanax-top.st:52736:StatusDetectorExecutor methodsFor: 'executing'!
{void} execute: arg {Int32}
	arg == Int32Zero ifTrue: [
		myWork removeLastStatusDetector]!
*/
}
public StatusDetectorExecutor(FeWork work) {
	super();
	myWork = work;
/*
udanax-top.st:52742:StatusDetectorExecutor methodsFor: 'protected: create'!
create: work {FeWork}
	super create.
	myWork := work.!
*/
}
public static XnExecutor make(FeWork work) {
	return new StatusDetectorExecutor(work);
/*
udanax-top.st:52755:StatusDetectorExecutor class methodsFor: 'create'!
{XnExecutor} make: work {FeWork}
	^ self create: work!
*/
}
public StatusDetectorExecutor() {
/*

Generated during transformation
*/
}
public StatusDetectorExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
