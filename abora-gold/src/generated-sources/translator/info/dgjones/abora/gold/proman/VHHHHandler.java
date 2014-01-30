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
import info.dgjones.abora.gold.java.missing.handle.VHHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.VHHHHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class VHHHHandler extends RequestHandler {

	protected VHHHFn myFn;
	protected Category myType1;
	protected Category myType2;
	protected Category myType3;
/*
udanax-top.st:44309:
RequestHandler subclass: #VHHHHandler
	instanceVariableNames: '
		myFn {VHHHFn var}
		myType1 {Category}
		myType2 {Category}
		myType3 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44317:
(VHHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44342:
VHHHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44345:
(VHHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VHHHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	Heaper arg2;
	Heaper arg3;
	arg1 = pm.fetchNonNullHeaper(myType1);
	arg2 = pm.fetchNonNullHeaper(myType2);
	arg3 = pm.fetchNonNullHeaper(myType3);
	if (pm.noErrors()) {
		myFn.invokeFunction(arg1, arg2, arg3);
		pm.respondVoid();
	}
/*
udanax-top.st:44322:VHHHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} arg3 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	arg3 _ pm fetchNonNullHeaper: myType3.
	pm noErrors ifTrue:
		[(myFn invokeFunction: arg1 with: arg2 with: arg3).
		pm respondVoid]!
*/
}
public VHHHHandler(VHHHFn fn, Category type1, Category type2, Category type3) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
	myType3 = type3;
/*
udanax-top.st:44334:VHHHHandler methodsFor: 'creation'!
create: fn {VHHHFn var} with: type1 {Category} with: type2 {Category} with: type3 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.
	myType3 _ type3.!
*/
}
public static RequestHandler make(VHHHFn fn, Category type1, Category type2, Category type3) {
	return new VHHHHandler(fn, type1, type2, type3);
/*
udanax-top.st:44350:VHHHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHHHFn var} with: type1 {Category} with: type2 {Category} with: type3 {Category}
	^self create: fn with: type1 with: type2 with: type3!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:44355:VHHHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public VHHHHandler() {
/*

Generated during transformation
*/
}
public VHHHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
