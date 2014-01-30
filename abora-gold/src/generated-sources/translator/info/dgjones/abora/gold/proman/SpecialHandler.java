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
import info.dgjones.abora.gold.proman.SpecialHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class SpecialHandler extends RequestHandler {

	protected VHFn myFn;
/*
udanax-top.st:44148:
RequestHandler subclass: #SpecialHandler
	instanceVariableNames: 'myFn {VHFn var}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:44152:
(SpecialHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:44168:
SpecialHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:44171:
(SpecialHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SpecialHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	myFn.invokeFunction(pm);
/*
udanax-top.st:44157:SpecialHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	myFn invokeFunction: pm!
*/
}
public SpecialHandler(VHFn fn) {
	super();
	myFn = fn;
/*
udanax-top.st:44163:SpecialHandler methodsFor: 'creation'!
create: fn {VHFn var}
	super create.
	myFn _ fn.!
*/
}
public static RequestHandler make(VHFn fn) {
	return new SpecialHandler(fn);
/*
udanax-top.st:44176:SpecialHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {VHFn var}
	^ self create: fn!
*/
}
public SpecialHandler() {
/*

Generated during transformation
*/
}
public SpecialHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
