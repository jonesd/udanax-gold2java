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
import info.dgjones.abora.gold.java.missing.handle.BHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.BHHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class BHHandler extends RequestHandler {

	protected BHFn myFn;
	protected Category myType1;
/*
udanax-top.st:43576:
RequestHandler subclass: #BHHandler
	instanceVariableNames: '
		myFn {BHFn var}
		myType1 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43582:
(BHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43602:
BHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43605:
(BHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	arg1 = pm.fetchNonNullHeaper(myType1);
	if (pm.noErrors()) {
		pm.respondBooleanVar((myFn.invokeFunction(arg1)));
	}
/*
udanax-top.st:43587:BHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	pm noErrors ifTrue:
		[pm respondBooleanVar: (myFn invokeFunction: arg1)]!
*/
}
public BHHandler(BHFn fn, Category type1) {
	super();
	myFn = fn;
	myType1 = type1;
/*
udanax-top.st:43596:BHHandler methodsFor: 'creation'!
create: fn {BHFn var} with: type1 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.!
*/
}
public static RequestHandler make(BHFn fn, Category type1) {
	return new BHHandler(fn, type1);
/*
udanax-top.st:43610:BHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {BHFn var} with: type1 {Category}
	^self create: fn with: type1!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43615:BHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public BHHandler() {
/*

Generated during transformation
*/
}
public BHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
