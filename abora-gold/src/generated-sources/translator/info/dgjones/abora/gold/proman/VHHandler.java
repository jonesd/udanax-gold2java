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
import info.dgjones.abora.gold.java.missing.handle.VHFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.VHHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class VHHandler extends RequestHandler {

	protected VHFn myFn;
	protected Category myType1;
/*
udanax-top.st:44222:
RequestHandler subclass: #VHHandler
	instanceVariableNames: '
		myFn {VHFn var}
		myType1 {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44228:
(VHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44249:
VHHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44252:
(VHHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VHHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	Heaper arg1;
	arg1 = pm.fetchNonNullHeaper(myType1);
	if (pm.noErrors()) {
		myFn.invokeFunction(arg1);
		pm.respondVoid();
	}
/*
udanax-top.st:44233:VHHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	| arg1 {Heaper} |
	arg1 _ pm fetchNonNullHeaper: myType1.
	pm noErrors ifTrue:
		[(myFn invokeFunction: arg1).
		pm respondVoid]!
*/
}
public VHHandler(VHFn fn, Category type1) {
	super();
	myFn = fn;
	myType1 = type1;
/*
udanax-top.st:44243:VHHandler methodsFor: 'creation'!
create: fn {VHFn var} with: type1 {Category}
	super create.
	myFn _ fn.
	myType1 _ type1.!
*/
}
public static RequestHandler make(VHFn fn, Category type1) {
	return new VHHandler(fn, type1);
/*
udanax-top.st:44257:VHHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHFn var} with: type1 {Category}
	^self create: fn with: type1!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:44262:VHHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public VHHandler() {
/*

Generated during transformation
*/
}
public VHHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
