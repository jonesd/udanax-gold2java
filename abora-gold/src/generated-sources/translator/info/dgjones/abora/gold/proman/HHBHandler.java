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
import info.dgjones.abora.gold.java.missing.handle.HHBFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.HHBHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HHBHandler extends RequestHandler {

	protected HHBFn myFn;
	protected Category myType1;
/*
udanax-top.st:43770:
RequestHandler subclass: #HHBHandler
	instanceVariableNames: '
		myFn {HHBFn var}
		myType1 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43776:
(HHBHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43797:
HHBHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43800:
(HHBHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HHBHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
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
		pm.respondHeaper((myFn.invokeFunction(arg1, arg2)));
	}
/*
udanax-top.st:43781:HHBHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {BooleanVar} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchBooleanVar.
	pm noErrors ifTrue:
		[pm respondHeaper: (myFn invokeFunction: arg1 with: arg2)]!
*/
}
public HHBHandler(HHBFn fn, Category type1) {
	super();
	myFn = fn;
	myType1 = type1;
/*
udanax-top.st:43791:HHBHandler methodsFor: 'creation'!
create: fn {HHBFn var} with: type1 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.!
*/
}
public static RequestHandler make(HHBFn fn, Category type1) {
	return new HHBHandler(fn, type1);
/*
udanax-top.st:43805:HHBHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {HHBFn var} with: type1 {Category}
	^self create: fn with: type1!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43810:HHBHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public HHBHandler() {
/*

Generated during transformation
*/
}
public HHBHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
