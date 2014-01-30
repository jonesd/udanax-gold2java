/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.DiskConnection;
import info.dgjones.abora.gold.cobbler.FromDiskPlan;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

/**
 * Instances of this represent the plan for getting a particular kind of object from an urdi
 * on a particular file.  They open the urdi, create a packer, retrieve the Turtle from the
 * packer, and pull out the boot object.
 */
public class FromDiskPlan extends BootPlan {

	protected Category myCategory;
	protected String myFilename;
/*
udanax-top.st:57091:
BootPlan subclass: #FromDiskPlan
	instanceVariableNames: '
		myCategory {Category}
		myFilename {Character star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:57097:
FromDiskPlan comment:
'Instances of this represent the plan for getting a particular kind of object from an urdi on a particular file.  They open the urdi, create a packer, retrieve the Turtle from the packer, and pull out the boot object.'!
*/
/*
udanax-top.st:57099:
(FromDiskPlan getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FromDiskPlan.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myCategory;
/*
udanax-top.st:57104:FromDiskPlan methodsFor: 'accessing'!
{Category} bootCategory
	^myCategory!
*/
}
/**
 * Return the object representing the connection.  This gives the client a handle by which to
 * terminate the connection.
 */
public Connection connection() {
	DiskManager.make(myFilename);
	return new DiskConnection(bootCategory(), ((DiskManager) CurrentPacker.fluidGet()).getInitialFlock().bootHeaper());
/*
udanax-top.st:57107:FromDiskPlan methodsFor: 'accessing'!
{Connection} connection
	"Return the object representing the connection.  This gives the client a handle by which to terminate the connection."
	
	DiskManager make: myFilename.
	^DiskConnection create: self bootCategory with: CurrentPacker fluidGet getInitialFlock bootHeaper!
*/
}
public FromDiskPlan(Category cat, String filename) {
	super();
	myCategory = cat;
	myFilename = filename;
/*
udanax-top.st:57115:FromDiskPlan methodsFor: 'creation'!
create: cat {Category} with: filename {Character star}
	super create.
	myCategory _ cat.
	myFilename _ filename!
*/
}
public FromDiskPlan(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
	myFilename = receiver.receiveString();
/*
udanax-top.st:57122:FromDiskPlan methodsFor: 'generated:'!
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
udanax-top.st:57127:FromDiskPlan methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.
	xmtr sendString: myFilename.!
*/
}
public FromDiskPlan() {
/*

Generated during transformation
*/
}
}
