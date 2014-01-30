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
import info.dgjones.abora.gold.java.missing.handle.VHBFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.VHBHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class VHBHandler extends RequestHandler {

	protected VHBFn myFn;
	protected Category myType1;
/*
udanax-top.st:44179:
RequestHandler subclass: #VHBHandler
	instanceVariableNames: '
		myFn {VHBFn var}
		myType1 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44185:
(VHBHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44207:
VHBHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44210:
(VHBHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VHBHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	boolean arg2;
	arg1 = pm.fetchNonNullHeaper(myType1);
	arg2 = pm.fetchBooleanVar();
	if (pm.noErrors()) {
		myFn.invokeFunction(arg1, arg2);
		pm.respondVoid();
	}
/*
udanax-top.st:44190:VHBHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {BooleanVar} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchBooleanVar.
	pm noErrors ifTrue:
		[(myFn invokeFunction: arg1 with: arg2).
		pm respondVoid]!
*/
}
public VHBHandler(VHBFn fn, Category type1) {
	super();
	myFn = fn;
	myType1 = type1;
/*
udanax-top.st:44201:VHBHandler methodsFor: 'creation'!
create: fn {VHBFn var} with: type1 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.!
*/
}
public static RequestHandler make(VHBFn fn, Category type1) {
	return new VHBHandler(fn, type1);
/*
udanax-top.st:44215:VHBHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHBFn var} with: type1 {Category}
	^self create: fn with: type1!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:44220:VHBHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public VHBHandler() {
/*

Generated during transformation
*/
}
public VHBHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
