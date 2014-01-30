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
import info.dgjones.abora.gold.java.missing.handle.VHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.VHHHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class VHHHandler extends RequestHandler {

	protected VHHFn myFn;
	protected Category myType1;
	protected Category myType2;
/*
udanax-top.st:44264:
RequestHandler subclass: #VHHHandler
	instanceVariableNames: '
		myFn {VHHFn var}
		myType1 {Category}
		myType2 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44271:
(VHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44294:
VHHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44297:
(VHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VHHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
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
		myFn.invokeFunction(arg1, arg2);
		pm.respondVoid();
	}
/*
udanax-top.st:44276:VHHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	pm noErrors ifTrue:
		[(myFn invokeFunction: arg1 with: arg2).
		pm respondVoid]!
*/
}
public VHHHandler(VHHFn fn, Category type1, Category type2) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
/*
udanax-top.st:44287:VHHHandler methodsFor: 'creation'!
create: fn {VHHFn var} with: type1 {Category} with: type2 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.!
*/
}
public static RequestHandler make(VHHFn fn, Category type1, Category type2) {
	return new VHHHandler(fn, type1, type2);
/*
udanax-top.st:44302:VHHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHHFn var} with: type1 {Category} with: type2 {Category}
	^self create: fn with: type1 with: type2!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:44307:VHHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public VHHHandler() {
/*

Generated during transformation
*/
}
public VHHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
