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
import info.dgjones.abora.gold.brange2.BeWorkLockExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class BeWorkLockExecutor extends XnExecutor {

	protected BeWork myWork;
/*
udanax-top.st:12910:
XnExecutor subclass: #BeWorkLockExecutor
	instanceVariableNames: 'myWork {BeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-brange2'!
*/
/*
udanax-top.st:12914:
(BeWorkLockExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:12930:
BeWorkLockExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:12933:
(BeWorkLockExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BeWorkLockExecutor.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * The work's locking pointer will already be NULL, so we only have to update
 */
public void execute(int estateIndex) {
	myWork.updateFeStatus();
/*
udanax-top.st:12919:BeWorkLockExecutor methodsFor: 'invoking'!
{void} execute: estateIndex {Int32 unused}
	"The work's locking pointer will already be NULL, so we only have to update"
	myWork updateFeStatus!
*/
}
public BeWorkLockExecutor(BeWork work) {
	super();
	myWork = work;
/*
udanax-top.st:12925:BeWorkLockExecutor methodsFor: 'create'!
create: work {BeWork}
	super create.
	myWork := work!
*/
}
public static BeWorkLockExecutor make(BeWork work) {
	return new BeWorkLockExecutor(work);
/*
udanax-top.st:12938:BeWorkLockExecutor class methodsFor: 'pseudoconstructors'!
make: work {BeWork}
	^ BeWorkLockExecutor create: work!
*/
}
public BeWorkLockExecutor() {
/*

Generated during transformation
*/
}
public BeWorkLockExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
