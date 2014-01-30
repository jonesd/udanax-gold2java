/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fm.support;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Thunk is the abstraction for reified void/0-argument operations.  Therse include Testers,
 * frontend operations, etc.
 */
public class Thunk extends Heaper {

/*
udanax-top.st:56776:
Heaper subclass: #Thunk
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'FM-Support'!
*/
/*
udanax-top.st:56780:
Thunk comment:
'Thunk is the abstraction for reified void/0-argument operations.  Therse include Testers, frontend operations, etc.'!
*/
/*
udanax-top.st:56782:
(Thunk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Thunk.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Execute the action defined by this thunk.
 */
public void execute() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:56787:Thunk methodsFor: 'operate'!
{void} execute
	"Execute the action defined by this thunk."
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:56794:Thunk methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:56796:Thunk methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public Thunk() {
/*

Generated during transformation
*/
}
public Thunk(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
