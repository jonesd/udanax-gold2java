/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cobbler;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.DiskConnection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Keep an object from the disk.  For the moment, put the disk connection in a global
 * variable and export a function so that anyone can destroy it....
 */
public class DiskConnection extends Connection {

	protected Category myCategory;
	protected Heaper myHeaper;
/*
udanax-top.st:13944:
Connection subclass: #DiskConnection
	instanceVariableNames: '
		myCategory {Category}
		myHeaper {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cobbler'!
*/
/*
udanax-top.st:13950:
DiskConnection comment:
'Keep an object from the disk.  For the moment, put the disk connection in a global variable and export a function so that anyone can destroy it....'!
*/
/*
udanax-top.st:13952:
(DiskConnection getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskConnection.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myCategory;
/*
udanax-top.st:13957:DiskConnection methodsFor: 'accessing'!
{Category} bootCategory
	^myCategory!
*/
}
public Heaper bootHeaper() {
	return myHeaper;
/*
udanax-top.st:13960:DiskConnection methodsFor: 'accessing'!
{Heaper} bootHeaper
	^myHeaper!
*/
}
public DiskConnection(Category cat, Heaper heaper) {
	super();
	myCategory = cat;
	myHeaper = heaper;
/*
udanax-top.st:13965:DiskConnection methodsFor: 'creation'!
create: cat {Category} with: heaper {Heaper}
	super create.
	myCategory _ cat.
	myHeaper _ heaper!
*/
}
public void destruct() {
	myHeaper = null;
	((DiskManager) CurrentPacker.fluidGet()).purge();
	((DiskManager) CurrentPacker.fluidGet()).destroy();
	CurrentPacker.fluidSet(((DiskManager) null));
	super.destruct();
/*
udanax-top.st:13970:DiskConnection methodsFor: 'creation'!
{void} destruct
	myHeaper _ NULL.
	CurrentPacker fluidGet purge.
	CurrentPacker fluidGet destroy.
	CurrentPacker fluidSet: (NULL basicCast: DiskManager).
	super destruct!
*/
}
public DiskConnection() {
/*

Generated during transformation
*/
}
public DiskConnection(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
