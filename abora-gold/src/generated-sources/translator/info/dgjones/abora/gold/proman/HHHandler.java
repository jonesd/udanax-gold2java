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
import info.dgjones.abora.gold.java.missing.handle.HHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.HHHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HHHandler extends RequestHandler {

	protected HHFn myFn;
	protected Category myType1;
/*
udanax-top.st:43812:
RequestHandler subclass: #HHHandler
	instanceVariableNames: '
		myFn {HHFn var}
		myType1 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43818:
(HHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43838:
HHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43841:
(HHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	arg1 = pm.fetchNonNullHeaper(myType1);
	if (pm.noErrors()) {
		pm.respondHeaper((myFn.invokeFunction(arg1)));
	}
/*
udanax-top.st:43823:HHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	pm noErrors ifTrue:
		[pm respondHeaper: (myFn invokeFunction: arg1)]!
*/
}
public HHHandler(HHFn fn, Category type1) {
	super();
	myFn = fn;
	myType1 = type1;
/*
udanax-top.st:43832:HHHandler methodsFor: 'creation'!
create: fn {HHFn var} with: type1 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.!
*/
}
public static RequestHandler make(HHFn fn, Category type1) {
	return new HHHandler(fn, type1);
/*
udanax-top.st:43846:HHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {HHFn var} with: type1 {Category}
	^self create: fn with: type1!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43851:HHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public HHHandler() {
/*

Generated during transformation
*/
}
public HHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
