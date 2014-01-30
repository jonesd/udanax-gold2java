/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.calc;

import info.dgjones.abora.gold.calc.TrackCBlocks;
import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.CBlockTrackingPacker;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

public class TrackCBlocks extends BootPlan {

	protected BootPlan myBootPlan;
/*
udanax-top.st:57132:
BootPlan subclass: #TrackCBlocks
	instanceVariableNames: 'myBootPlan {BootPlan}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-calc'!
*/
/*
udanax-top.st:57136:
(TrackCBlocks getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TrackCBlocks.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myBootPlan.bootCategory();
/*
udanax-top.st:57141:TrackCBlocks methodsFor: 'accessing'!
{Category} bootCategory
	^myBootPlan bootCategory!
*/
}
/**
 * Return the object representing the connection. This gives the client a handle by
 * which to terminate the connection.
 */
public Connection connection() {
	Connection result;
	result = myBootPlan.connection();
	CurrentPacker.fluidSet((CBlockTrackingPacker.make(((DiskManager) CurrentPacker.fluidGet()))));
	return result;
/*
udanax-top.st:57145:TrackCBlocks methodsFor: 'accessing'!
{Connection} connection
	"Return the object representing the connection. This gives the client a handle by 
	which to terminate the connection."
	
	| result {Connection} |
	result _ myBootPlan connection.
	CurrentPacker fluidSet: (CBlockTrackingPacker make: CurrentPacker fluidGet).
	^result!
*/
}
public TrackCBlocks(Rcvr receiver) {
	super(receiver);
	myBootPlan = (BootPlan) receiver.receiveHeaper();
/*
udanax-top.st:57156:TrackCBlocks methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myBootPlan _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myBootPlan);
/*
udanax-top.st:57160:TrackCBlocks methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myBootPlan.!
*/
}
public TrackCBlocks() {
/*

Generated during transformation
*/
}
}
