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
import info.dgjones.abora.gold.java.missing.handle.BHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.BHHHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class BHHHandler extends RequestHandler {

	protected BHHFn myFn;
	protected Category myType1;
	protected Category myType2;
/*
udanax-top.st:43617:
RequestHandler subclass: #BHHHandler
	instanceVariableNames: '
		myFn {BHHFn var}
		myType1 {Category}
		myType2 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43624:
(BHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43646:
BHHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43649:
(BHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BHHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	Heaper arg2;
	arg1 = pm.fetchNonNullHeaper(myType1);
	arg2 = pm.fetchNonNullHeaper(myType2);
	if (pm.noErrors()) {
		pm.respondBooleanVar((myFn.invokeFunction(arg1, arg2)));
	}
/*
udanax-top.st:43629:BHHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	pm noErrors ifTrue:
		[pm respondBooleanVar: (myFn invokeFunction: arg1 with: arg2)]!
*/
}
public BHHHandler(BHHFn fn, Category type1, Category type2) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
/*
udanax-top.st:43639:BHHHandler methodsFor: 'creation'!
create: fn {BHHFn var} with: type1 {Category} with: type2 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.!
*/
}
public static RequestHandler make(BHHFn fn, Category type1, Category type2) {
	return new BHHHandler(fn, type1, type2);
/*
udanax-top.st:43654:BHHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {BHHFn var} with: type1 {Category} with: type2 {Category}
	^self create: fn with: type1 with: type2!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43659:BHHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public BHHHandler() {
/*

Generated during transformation
*/
}
public BHHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
