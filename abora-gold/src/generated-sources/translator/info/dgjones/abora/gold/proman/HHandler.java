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
import info.dgjones.abora.gold.java.missing.handle.HFn;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.HHandler;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class HHandler extends RequestHandler {

	protected HFn myFn;
/*
udanax-top.st:43734:
RequestHandler subclass: #HHandler
	instanceVariableNames: 'myFn {HFn var}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:43738:
(HHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:43755:
HHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:43758:
(HHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HHandler.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	if (pm.noErrors()) {
		pm.respondHeaper((myFn.invokeFunction()));
	}
/*
udanax-top.st:43743:HHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	
	pm noErrors ifTrue:
		[pm respondHeaper: (myFn invokeFunction)]!
*/
}
public HHandler(HFn fn) {
	super();
	myFn = fn;
/*
udanax-top.st:43750:HHandler methodsFor: 'creation'!
create: fn {HFn var}
	super create.
	myFn _ fn.!
*/
}
public static RequestHandler make(HFn fn) {
	return new HHandler(fn);
/*
udanax-top.st:43763:HHandler class methodsFor: 'creation'!
{RequestHandler} make: fn {HFn var}
	^self create: fn!
*/
}
public static boolean isGenerated() {
	return true;
/*
udanax-top.st:43768:HHandler class methodsFor: 'generated:'!
isGenerated ^true!
*/
}
public HHandler() {
/*

Generated during transformation
*/
}
public HHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
