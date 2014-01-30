/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wparray;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class XnExecutor extends Heaper {

	protected static XnExecutor TheNoopExecutor;
/*
Xanadu-wparray.st:0:
Heaper subclass: #XnExecutor
	instanceVariableNames: ''
	classVariableNames: 'TheNoopExecutor {XnExecutor} '
	poolDictionaries: ''
	category: 'Xanadu-wparray'!
*/
/*
Xanadu-wparray.st:4:
(XnExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
Xanadu-wparray.st:19:
XnExecutor class
	instanceVariableNames: ''!
*/
/*
Xanadu-wparray.st:22:
(XnExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XnExecutor.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * superclass implements a noop
 */
public void execute(int estateIndex) {
/*
Xanadu-wparray.st:9:XnExecutor methodsFor: 'invoking'!
{void} execute: estateIndex {Int32}
	"superclass implements a noop"!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
Xanadu-wparray.st:14:XnExecutor methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
Xanadu-wparray.st:16:XnExecutor methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void linkTimeNonInherited() {
	TheNoopExecutor = null;
/*
Xanadu-wparray.st:27:XnExecutor class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheNoopExecutor := NULL!
*/
}
public static void TimeNonInherited() {
	TheNoopExecutor = new XnExecutor();
/*
Xanadu-wparray.st:30:XnExecutor class methodsFor: 'smalltalk: init'!
TimeNonInherited
	TheNoopExecutor := XnExecutor create!
*/
}
public static XnExecutor noopExecutor() {
	return TheNoopExecutor;
/*
Xanadu-wparray.st:35:XnExecutor class methodsFor: 'pseudoconstructor'!
{XnExecutor} noopExecutor
	^ TheNoopExecutor!
*/
}
public XnExecutor() {
/*

Generated during transformation
*/
}
public XnExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
