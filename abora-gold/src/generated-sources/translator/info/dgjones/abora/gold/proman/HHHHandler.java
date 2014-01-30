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
import info.dgjones.abora.gold.java.missing.handle.HHHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.HHHHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HHHHandler extends RequestHandler {

	protected HHHFn myFn;
	protected Category myType1;
	protected Category myType2;
/*
udanax-top.st:43898:
RequestHandler subclass: #HHHHandler
	instanceVariableNames: '
		myFn {HHHFn var}
		myType1 {Category}
		myType2 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43905:
(HHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43927:
HHHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43930:
(HHHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HHHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
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
		pm.respondHeaper((myFn.invokeFunction(arg1, arg2)));
	}
/*
udanax-top.st:43910:HHHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} arg2 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	arg2 _ pm fetchNonNullHeaper: myType2.
	pm noErrors ifTrue:
		[pm respondHeaper: (myFn invokeFunction: arg1 with: arg2)]!
*/
}
public HHHHandler(HHHFn fn, Category type1, Category type2) {
	super();
	myFn = fn;
	myType1 = type1;
	myType2 = type2;
/*
udanax-top.st:43920:HHHHandler methodsFor: 'creation'!
create: fn {HHHFn var} with: type1 {Category} with: type2 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.
	myType2 _ type2.!
*/
}
public static RequestHandler make(HHHFn fn, Category type1, Category type2) {
	return new HHHHandler(fn, type1, type2);
/*
udanax-top.st:43935:HHHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {HHHFn var} with: type1 {Category} with: type2 {Category}
	^self create: fn with: type1 with: type2!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43940:HHHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public HHHHandler() {
/*

Generated during transformation
*/
}
public HHHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
