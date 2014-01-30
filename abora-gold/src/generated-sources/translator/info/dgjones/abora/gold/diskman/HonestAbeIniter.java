/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.diskman;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.diskman.HonestAbeIniter;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.TestPacker;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

public class HonestAbeIniter extends Thunk {

	protected Category myCategory;
	protected boolean blastOnError;
	protected int persistInterval;
	protected static Connection TheHonestConnection;
	protected static BeGrandMap TheHonestGrandMap;
/*
udanax-top.st:57316:
Thunk subclass: #HonestAbeIniter
	instanceVariableNames: '
		myCategory {Category}
		blastOnError {BooleanVar}
		persistInterval {IntegerVar}'
	classVariableNames: '
		TheHonestConnection {Connection} 
		TheHonestGrandMap {BeGrandMap} '
	poolDictionaries: ''
	category: 'Xanadu-diskman'!
*/
/*
udanax-top.st:57325:
(HonestAbeIniter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:57362:
HonestAbeIniter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:57365:
(HonestAbeIniter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HonestAbeIniter.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	Cookbook cookbook;
	Turtle turtle;
	Connection conn;
	TestPacker.make(blastOnError, persistInterval);
	cookbook = Cookbook.makeCategory(myCategory);
	turtle = Turtle.make(cookbook, myCategory, ProtocolBroker.diskProtocol());
	conn = Connection.make(myCategory);
	TheHonestConnection = conn;
	turtle.saveBootHeaper(conn.bootHeaper());
	/* The following is here so that later thunks can get the GrandMap &c */
	/* [WorksBootMaker] USES.
	GrandConnection fluidSet: TheHonestConnection.� */
	TheHonestGrandMap = (BeGrandMap) conn.bootHeaper();
	((DiskManager) CurrentPacker.fluidGet()).purge();
	/* CurrentPacker fluidSet: NULL.
 */
/*
udanax-top.st:57330:HonestAbeIniter methodsFor: 'running'!
{void} execute
	| cookbook {Cookbook} turtle {Turtle} conn {Connection} |
	TestPacker make: blastOnError with: persistInterval.
	cookbook := Cookbook make.Category: myCategory.
	turtle := Turtle make: cookbook with: myCategory with: ProtocolBroker diskProtocol.
	conn := Connection make: myCategory.
	TheHonestConnection _ conn.
	turtle saveBootHeaper: conn bootHeaper.
	"The following is here so that later thunks can get the GrandMap &c"
	"[WorksBootMaker] USES.
	GrandConnection fluidSet: TheHonestConnection.�"
	TheHonestGrandMap _ conn bootHeaper cast: BeGrandMap.
	CurrentPacker fluidGet purge.
	"CurrentPacker fluidSet: NULL.
"!
*/
}
public HonestAbeIniter(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
	blastOnError = receiver.receiveBooleanVar();
	persistInterval = receiver.receiveIntegerVar();
/*
udanax-top.st:57349:HonestAbeIniter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCategory _ receiver receiveHeaper.
	blastOnError _ receiver receiveBooleanVar.
	persistInterval _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCategory);
	xmtr.sendBooleanVar(blastOnError);
	xmtr.sendIntegerVar(persistInterval);
/*
udanax-top.st:57355:HonestAbeIniter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.
	xmtr sendBooleanVar: blastOnError.
	xmtr sendIntegerVar: persistInterval.!
*/
}
public static BeGrandMap fetchGrandMap() {
	return TheHonestGrandMap;
/*
udanax-top.st:57370:HonestAbeIniter class methodsFor: 'accessing'!
{BeGrandMap} fetchGrandMap
	^ TheHonestGrandMap!
*/
}
public static void exitTimeNonInherited() {
	TheHonestConnection = null;
/*
udanax-top.st:57375:HonestAbeIniter class methodsFor: 'smalltalk: init'!
exitTimeNonInherited
	TheHonestConnection _ NULL!
*/
}
public static void linkTimeNonInherited() {
	TheHonestConnection = null;
	TheHonestGrandMap = null;
/*
udanax-top.st:57378:HonestAbeIniter class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheHonestConnection _ NULL.
	TheHonestGrandMap _ NULL.!
*/
}
public HonestAbeIniter() {
/*

Generated during transformation
*/
}
}
