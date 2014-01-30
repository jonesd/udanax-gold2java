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
import info.dgjones.abora.gold.java.missing.handle.HIHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.ExampleHIHHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;

public class ExampleHIHHandler extends RequestHandler {

	protected HIHFn myFn;
	protected Category myType2;
/*
udanax-top.st:43708:
RequestHandler subclass: #ExampleHIHHandler
	instanceVariableNames: '
		myFn {HIHFn}
		myType2 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43714:
(ExampleHIHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #SMALLTALK.ONLY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ExampleHIHHandler.class).setAttributes( new Set().add("CONCRETE").add("SMALLTALKONLY"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:43719:ExampleHIHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	| arg1 {IntegerVar} arg2 {Heaper} |
	arg1 _ self fetchIntegerVar.
	arg2 _ self fetchHeaper: myType2.
	pm noErrors ifTrue:
		[self respondHeaper: (myFn invokeFunction: arg1 with: arg2)]!
*/
}
public ExampleHIHHandler(HIHFn fn, Category type2) {
	super();
	myFn = fn;
	myType2 = type2;
/*
udanax-top.st:43729:ExampleHIHHandler methodsFor: 'creation'!
create: fn {HIHFn} with: type2 {Category}
	super create.
	myFn _ fn.
	myType2 _ type2!
*/
}
public ExampleHIHHandler() {
/*

Generated during transformation
*/
}
public ExampleHIHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
