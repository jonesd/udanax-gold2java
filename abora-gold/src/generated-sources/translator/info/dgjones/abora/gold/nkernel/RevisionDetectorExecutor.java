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
import info.dgjones.abora.gold.nkernel.RevisionDetectorExecutor;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * This class informs its work when its last detector has gone away.
 */
public class RevisionDetectorExecutor extends XnExecutor {

	protected FeWork myWork;
/*
udanax-top.st:44826:
XnExecutor subclass: #RevisionDetectorExecutor
	instanceVariableNames: 'myWork {FeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:44830:
RevisionDetectorExecutor comment:
'This class informs its work when its last detector has gone away.'!
*/
/*
udanax-top.st:44832:
(RevisionDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44848:
RevisionDetectorExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44851:
(RevisionDetectorExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RevisionDetectorExecutor.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public RevisionDetectorExecutor(FeWork work) {
	super();
	myWork = work;
/*
udanax-top.st:44837:RevisionDetectorExecutor methodsFor: 'protected: create'!
create: work {FeWork}
	super create.
	myWork := work!
*/
}
public void execute(int arg) {
	if (arg == 0) {
		myWork.removeLastRevisionDetector();
	}
/*
udanax-top.st:44843:RevisionDetectorExecutor methodsFor: 'execute'!
{void} execute: arg {Int32}
	arg == Int32Zero ifTrue: [
		myWork removeLastRevisionDetector]!
*/
}
public static XnExecutor make(FeWork work) {
	return new RevisionDetectorExecutor(work);
/*
udanax-top.st:44856:RevisionDetectorExecutor class methodsFor: 'create'!
{XnExecutor} make: work {FeWork}
	^ self create: work!
*/
}
public RevisionDetectorExecutor() {
/*

Generated during transformation
*/
}
public RevisionDetectorExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
