/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.xcvr.DiskIniter;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

public class DiskIniter extends Thunk {

	protected Category myCategory;
	protected String myFilename;
	protected int mySnarfSize;
	protected int mySnarfCount;
	protected int myStageCount;
/*
udanax-top.st:57190:
Thunk subclass: #DiskIniter
	instanceVariableNames: '
		myCategory {Category}
		myFilename {char star}
		mySnarfSize {Int32 NOCOPY}
		mySnarfCount {Int32 NOCOPY}
		myStageCount {Int32 NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:57199:
(DiskIniter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskIniter.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	XcvrMaker maker;
	Cookbook cookbook;
	Turtle turtle;
	Connection conn;
	DiskManager.initializeDisk(myFilename);
	maker = ProtocolBroker.diskProtocol();
	cookbook = Cookbook.makeCategory(myCategory);
	turtle = Turtle.make(cookbook, myCategory, maker);
	conn = Connection.make(myCategory);
	turtle.saveBootHeaper(conn.bootHeaper());
	((BeGrandMap) conn.bootHeaper()).bePurgeable();
	((DiskManager) CurrentPacker.fluidGet()).purge();
	/* Let's make sure that the GC gets as much as possible. */
	/* [WorksBootMaker] USES.
	GrandConnection fluidSet: NULL. */
	conn = null;
	turtle = null;
	maker = null;
	cookbook = null;
	((DiskManager) CurrentPacker.fluidGet()).destroy();
	CurrentPacker.fluidSet(((DiskManager) null));
/*
udanax-top.st:57204:DiskIniter methodsFor: 'running'!
{void} execute
	| maker {XcvrMaker} cookbook {Cookbook} turtle {Turtle} conn {Connection} |
	DiskManager initializeDisk: myFilename.
	maker _ ProtocolBroker diskProtocol.
	cookbook _ Cookbook make.Category: myCategory.
	turtle _ Turtle make: cookbook with: myCategory with: maker.
	conn _ Connection make: myCategory.
	turtle saveBootHeaper: conn bootHeaper.
	(conn bootHeaper cast: BeGrandMap) bePurgeable.
	CurrentPacker fluidGet purge.
	"Let's make sure that the GC gets as much as possible."
	"[WorksBootMaker] USES.
	GrandConnection fluidSet: NULL."
	conn _ NULL.
	turtle _ NULL.
	maker _ NULL.
	cookbook _ NULL.
	CurrentPacker fluidGet destroy.
	CurrentPacker fluidSet: (NULL basicCast: DiskManager).!
*/
}
public DiskIniter(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
	myFilename = receiver.receiveString();
/*
udanax-top.st:57227:DiskIniter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCategory _ receiver receiveHeaper.
	myFilename _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCategory);
	xmtr.sendString(myFilename);
/*
udanax-top.st:57232:DiskIniter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.
	xmtr sendString: myFilename.!
*/
}
public DiskIniter() {
/*

Generated during transformation
*/
}
}
