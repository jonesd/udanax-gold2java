/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fbtest;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.fbtest.WorksBootMaker;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.fluid.Emulsion;

public class WorksBootMaker extends BootMaker {

/*
udanax-top.st:56977:
BootMaker subclass: #WorksBootMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fbtest'!
*/
/*
udanax-top.st:56981:
(WorksBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:57010:
WorksBootMaker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:57013:
(WorksBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksBootMaker.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return AboraSupport.findCategory(FeServer.class);
/*
udanax-top.st:56986:WorksBootMaker methodsFor: 'accessing'!
{Category} bootCategory
	^FeServer!
*/
}
public Heaper bootHeaper() {
	if (((Connection) GrandConnection.fluidFetch()) == null) {
		/* CurrentGrandMap fluidFetch == NULL ifFalse: [Heaper BLAST: #GrandMapWithoutConnection]. */
		GrandConnection.fluidSet((Connection.make(AboraSupport.findCategory(BeGrandMap.class))));
		CurrentGrandMap.fluidSet(((BeGrandMap) ((Connection) GrandConnection.fluidGet()).bootHeaper()));
		/* force agenda items to be invoked - they were commented out in getInitialFlock /ravi/10/22/92/ */
		AboraBlockSupport.enterConsistent();
		try {
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return FeServer.make();
/*
udanax-top.st:56991:WorksBootMaker methodsFor: 'protected:'!
{Heaper} bootHeaper
	GrandConnection fluidFetch == NULL ifTrue:
		["CurrentGrandMap fluidFetch == NULL ifFalse: [Heaper BLAST: #GrandMapWithoutConnection]."
		GrandConnection fluidSet: (Connection make: BeGrandMap).
		CurrentGrandMap fluidSet: (GrandConnection fluidGet bootHeaper cast: BeGrandMap).
		"force agenda items to be invoked - they were commented out in getInitialFlock /ravi/10/22/92/"
		DiskManager consistent: []].
	^FeServer make!
*/
}
public WorksBootMaker(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:57003:WorksBootMaker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:57006:WorksBootMaker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static void staticTimeNonInherited() {
	AboraSupport.defineFluid(Connection.class, "GrandConnection", Emulsion.globalEmulsion(), null);
/*
udanax-top.st:57018:WorksBootMaker class methodsFor: 'smalltalk: init'!
staticTimeNonInherited
	Connection defineFluid: #GrandConnection with: Emulsion globalEmulsion with: [NULL].!
*/
}
public WorksBootMaker() {
/*

Generated during transformation
*/
}
}
