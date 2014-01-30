/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fbtest;

import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.fbtest.FeWorksBootMaker;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

public class FeWorksBootMaker extends BootPlan {

/*
udanax-top.st:57065:
BootPlan subclass: #FeWorksBootMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fbtest'!
*/
/*
udanax-top.st:57069:
(FeWorksBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWorksBootMaker.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return AboraSupport.findCategory(FeServer.class);
/*
udanax-top.st:57074:FeWorksBootMaker methodsFor: 'accessing'!
{Category} bootCategory
	^ FeServer!
*/
}
public Connection connection() {
	Connection conn;
	conn = Connection.make(AboraSupport.findCategory(FeServer.class));
	return conn;
/*
udanax-top.st:57077:FeWorksBootMaker methodsFor: 'accessing'!
{Connection} connection
	| conn {Connection} |
	conn _ Connection make: FeServer.
	^ conn
	"^NestedConnection make: self bootCategory with: (PrGateKeeper make: (conn bootHeaper cast: FeGateKeeper)) with: conn"!
*/
}
public FeWorksBootMaker(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:57085:FeWorksBootMaker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:57088:FeWorksBootMaker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public FeWorksBootMaker() {
/*

Generated during transformation
*/
}
}
