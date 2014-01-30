/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.diskman;

import info.dgjones.abora.gold.diskman.Cattleman;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Remove flocks from the snarfpacker
 */
public class Cattleman extends XnExecutor {

	protected DiskManager myPasture;
/*
udanax-top.st:13184:
XnExecutor subclass: #Cattleman
	instanceVariableNames: 'myPasture {DiskManager}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-diskman'!
*/
/*
udanax-top.st:13188:
Cattleman comment:
'Remove flocks from the snarfpacker'!
*/
/*
udanax-top.st:13190:
(Cattleman getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:13209:
Cattleman class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13212:
(Cattleman getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Cattleman.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Cattleman(DiskManager dm) {
	super();
	myPasture = dm;
/*
udanax-top.st:13195:Cattleman methodsFor: 'create'!
create: dm {DiskManager}
	super create.
	myPasture := dm!
*/
}
/**
 * [Drops add: token] smalltalkOnly.
 */
public void execute(int token) {
	if (Heaper.isConstructed(myPasture)) {
		AboraSupport.smalltalkOnly();
		{
			Heaper.setGC(true);
		}
		myPasture.dropFlock(token);
		AboraSupport.smalltalkOnly();
		{
			Heaper.setGC(false);
		}
	}
/*
udanax-top.st:13201:Cattleman methodsFor: 'invoking'!
{void} execute: token {Int32}
	"[Drops add: token] smalltalkOnly."
	(Heaper isConstructed: myPasture) ifTrue: [
		[Heaper setGC: true] smalltalkOnly.
		myPasture dropFlock: token.
		[Heaper setGC: false] smalltalkOnly]!
*/
}
public static Cattleman make(DiskManager dm) {
	return new Cattleman(dm);
/*
udanax-top.st:13217:Cattleman class methodsFor: 'create'!
make: dm {DiskManager}
	^ self create: dm!
*/
}
public Cattleman() {
/*

Generated during transformation
*/
}
public Cattleman(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
