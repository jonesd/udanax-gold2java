/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.primtab.PrimSetExecutor;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class PrimSetExecutor extends XnExecutor {

	protected PrimSet mySet;
/*
udanax-top.st:34034:
XnExecutor subclass: #PrimSetExecutor
	instanceVariableNames: 'mySet {PrimSet}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:34038:
(PrimSetExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:34053:
PrimSetExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34056:
(PrimSetExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimSetExecutor.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public PrimSetExecutor(PrimSet set) {
	super();
	mySet = set;
/*
udanax-top.st:34043:PrimSetExecutor methodsFor: 'protected: create'!
create: set {PrimSet}
	super create.
	mySet := set!
*/
}
public void execute(int estateIndex) {
	mySet.weakRemove(estateIndex);
/*
udanax-top.st:34049:PrimSetExecutor methodsFor: 'execution'!
{void} execute: estateIndex {Int32}
	mySet weakRemove: estateIndex!
*/
}
public static PrimSetExecutor make(PrimSet set) {
	return new PrimSetExecutor(set);
/*
udanax-top.st:34061:PrimSetExecutor class methodsFor: 'pseudoconstructor'!
make: set {PrimSet}
	^ self create: set!
*/
}
public PrimSetExecutor() {
/*

Generated during transformation
*/
}
public PrimSetExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
