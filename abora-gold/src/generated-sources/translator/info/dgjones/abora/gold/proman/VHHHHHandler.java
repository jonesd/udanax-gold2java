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
import info.dgjones.abora.gold.java.missing.handle.VHHHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.VHHHHHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class VHHHHHandler extends RequestHandler {

	protected VHHHHFn myFn;
	protected Category myType1;
	protected Category myType2;
	protected Category myType3;
	protected Category myType4;
/*
udanax-top.st:44357:
RequestHandler subclass: #VHHHHHandler
	instanceVariableNames: '
		myFn {VHHHHFn var}
		myType1 {Category}
		myType2 {Category}
		myType3 {Category}
		myType4 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44366:
(VHHHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44393:
VHHHHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44396:
(VHHHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VHHHHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	Heaper arg2;
	Heaper arg3;
	Heaper arg4;
	arg1 = pm.fetchNonNullHeaper(myType1);
	arg2 = pm.fetchNonNullHeaper(myType2);
	arg3 = pm.fetchNonNullHeaper(myType3);
	arg4 = pm.fetchNonNullHeaper(myType4);
	if (pm.noErrors()) {
		myFn.invokeFunction(arg1, arg2, arg3, arg4);
		pm.respondVoid();
	}
/*
udanax-top.st:44371:VHHHHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} arg3 {Heaper} arg4 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	arg3 _ pm fetchNonNullHeaper: myType3.
	arg4 _ pm fetchNonNullHeaper: myType4.
	pm noErrors ifTrue:
		[(myFn invokeFunction: arg1 with: arg2 with: arg3 with: arg4).
		pm respondVoid]!
*/
}
public VHHHHHandler(VHHHHFn fn, Category type1, Category type2, Category type3, Category type4) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
	myType3 = type3;
	myType4 = type4;
/*
udanax-top.st:44384:VHHHHHandler methodsFor: 'creation'!
create: fn {VHHHHFn var} with: type1 {Category} with: type2 {Category} with: type3 {Category} with: type4 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.
	myType3 _ type3.
	myType4 _ type4.!
*/
}
public static RequestHandler make(VHHHHFn fn, Category type1, Category type2, Category type3, Category type4) {
	return new VHHHHHandler(fn, type1, type2, type3, type4);
/*
udanax-top.st:44401:VHHHHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHHHHFn var} with: type1 {Category} with: type2 {Category} with: type3 {Category} with: type4 {Category}
	^self create: fn with: type1 with: type2 with: type3 with: type4!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:44406:VHHHHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public VHHHHHandler() {
/*

Generated during transformation
*/
}
public VHHHHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
