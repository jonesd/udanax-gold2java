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
import info.dgjones.abora.gold.java.missing.handle.VHHHHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.VHHHHHHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class VHHHHHHandler extends RequestHandler {

	protected VHHHHHFn myFn;
	protected Category myType1;
	protected Category myType2;
	protected Category myType3;
	protected Category myType4;
	protected Category myType5;
/*
udanax-top.st:44408:
RequestHandler subclass: #VHHHHHHandler
	instanceVariableNames: '
		myFn {VHHHHHFn var}
		myType1 {Category}
		myType2 {Category}
		myType3 {Category}
		myType4 {Category}
		myType5 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44418:
(VHHHHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44447:
VHHHHHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44450:
(VHHHHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VHHHHHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	Heaper arg2;
	Heaper arg3;
	Heaper arg4;
	Heaper arg5;
	arg1 = pm.fetchNonNullHeaper(myType1);
	arg2 = pm.fetchNonNullHeaper(myType2);
	arg3 = pm.fetchNonNullHeaper(myType3);
	arg4 = pm.fetchNonNullHeaper(myType4);
	arg5 = pm.fetchNonNullHeaper(myType5);
	if (pm.noErrors()) {
		myFn.invokeFunction(arg1, arg2, arg3, arg4, arg5);
		pm.respondVoid();
	}
/*
udanax-top.st:44423:VHHHHHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} arg3 {Heaper} arg4 {Heaper} arg5 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	arg3 _ pm fetchNonNullHeaper: myType3.
	arg4 _ pm fetchNonNullHeaper: myType4.
	arg5 _ pm fetchNonNullHeaper: myType5.
	pm noErrors ifTrue:
		[(myFn invokeFunction: arg1 with: arg2 with: arg3 with: arg4 with: arg5).
		pm respondVoid]!
*/
}
public VHHHHHHandler(VHHHHHFn fn, Category type1, Category type2, Category type3, Category type4, Category type5) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
	myType3 = type3;
	myType4 = type4;
	myType5 = type5;
/*
udanax-top.st:44437:VHHHHHHandler methodsFor: 'creation'!
create: fn {VHHHHHFn var} with: type1 {Category} with: type2 {Category} with: type3 {Category} with: type4 {Category} with: type5 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.
	myType3 _ type3.
	myType4 _ type4.
	myType5 _ type5.!
*/
}
public static RequestHandler make(VHHHHHFn fn, Category type1, Category type2, Category type3, Category type4, Category type5) {
	return new VHHHHHHandler(fn, type1, type2, type3, type4, type5);
/*
udanax-top.st:44455:VHHHHHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHHHHHFn var} with: type1 {Category} with: type2 {Category} with: type3 {Category} with: type4 {Category} with: type5 {Category}
	^self create: fn with: type1 with: type2 with: type3 with: type4 with: type5!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:44460:VHHHHHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public VHHHHHHandler() {
/*

Generated during transformation
*/
}
public VHHHHHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
