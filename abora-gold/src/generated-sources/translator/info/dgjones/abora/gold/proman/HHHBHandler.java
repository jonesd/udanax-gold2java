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
import info.dgjones.abora.gold.java.missing.handle.HHHBFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.HHHBHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HHHBHandler extends RequestHandler {

	protected HHHBFn myFn;
	protected Category myType1;
	protected Category myType2;
/*
udanax-top.st:43853:
RequestHandler subclass: #HHHBHandler
	instanceVariableNames: '
		myFn {HHHBFn var}
		myType1 {Category}
		myType2 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43860:
(HHHBHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43883:
HHHBHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43886:
(HHHBHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HHHBHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	Heaper arg2;
	boolean arg3;
	arg1 = pm.fetchNonNullHeaper(myType1);
	arg2 = pm.fetchNonNullHeaper(myType2);
	arg3 = pm.fetchBooleanVar();
	if (pm.noErrors()) {
		pm.respondHeaper((myFn.invokeFunction(arg1, arg2, arg3)));
	}
/*
udanax-top.st:43865:HHHBHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} arg3 {BooleanVar} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	arg3 _ pm fetchBooleanVar.
	pm noErrors ifTrue:
		[pm respondHeaper: (myFn invokeFunction: arg1 with: arg2 with: arg3)]!
*/
}
public HHHBHandler(HHHBFn fn, Category type1, Category type2) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
/*
udanax-top.st:43876:HHHBHandler methodsFor: 'creation'!
create: fn {HHHBFn var} with: type1 {Category} with: type2 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.!
*/
}
public static RequestHandler make(HHHBFn fn, Category type1, Category type2) {
	return new HHHBHandler(fn, type1, type2);
/*
udanax-top.st:43891:HHHBHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {HHHBFn var} with: type1 {Category} with: type2 {Category}
	^self create: fn with: type1 with: type2!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43896:HHHBHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public HHHBHandler() {
/*

Generated during transformation
*/
}
public HHHBHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
