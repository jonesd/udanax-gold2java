/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.brange2;

import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.brange2.RevisionWatcherExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * This executor tells its BeWork when the last of its revision watchers have gone away.
 */
public class RevisionWatcherExecutor extends XnExecutor {

	protected BeWork myWork;
/*
udanax-top.st:44859:
XnExecutor subclass: #RevisionWatcherExecutor
	instanceVariableNames: 'myWork {BeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-brange2'!
*/
/*
udanax-top.st:44863:
RevisionWatcherExecutor comment:
'This executor tells its BeWork when the last of its revision watchers have gone away.'!
*/
/*
udanax-top.st:44865:
(RevisionWatcherExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44881:
RevisionWatcherExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44884:
(RevisionWatcherExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RevisionWatcherExecutor.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public RevisionWatcherExecutor(BeWork work) {
	super();
	myWork = work;
/*
udanax-top.st:44870:RevisionWatcherExecutor methodsFor: 'protected: create'!
create: work {BeWork}
	super create.
	myWork := work!
*/
}
public void execute(int arg) {
	if (arg == 0) {
		myWork.removeLastRevisionWatcher();
	}
/*
udanax-top.st:44876:RevisionWatcherExecutor methodsFor: 'execute'!
{void} execute: arg {Int32}
	arg == Int32Zero ifTrue: [
		myWork removeLastRevisionWatcher]!
*/
}
public static XnExecutor make(BeWork work) {
	return new RevisionWatcherExecutor(work);
/*
udanax-top.st:44889:RevisionWatcherExecutor class methodsFor: 'create'!
{XnExecutor} make: work {BeWork}
	^ self create: work!
*/
}
public RevisionWatcherExecutor() {
/*

Generated during transformation
*/
}
public RevisionWatcherExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
