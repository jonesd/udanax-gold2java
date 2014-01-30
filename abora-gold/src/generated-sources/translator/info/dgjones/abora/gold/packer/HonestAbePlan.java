/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.packer;

import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.packer.HonestAbePlan;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class HonestAbePlan extends BootMaker {

	protected Category myCategory;
/*
udanax-top.st:56914:
BootMaker subclass: #HonestAbePlan
	instanceVariableNames: 'myCategory {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-packer'!
*/
/*
udanax-top.st:56918:
(HonestAbePlan getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HonestAbePlan.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return myCategory;
/*
udanax-top.st:56923:HonestAbePlan methodsFor: 'accessing'!
{Category} bootCategory
	^myCategory!
*/
}
public Heaper bootHeaper() {
	return ((DiskManager) CurrentPacker.fluidGet()).getInitialFlock().bootHeaper();
/*
udanax-top.st:56926:HonestAbePlan methodsFor: 'accessing'!
{Heaper} bootHeaper
	^CurrentPacker fluidGet getInitialFlock bootHeaper!
*/
}
public HonestAbePlan(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
/*
udanax-top.st:56932:HonestAbePlan methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCategory _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCategory);
/*
udanax-top.st:56936:HonestAbePlan methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.!
*/
}
public HonestAbePlan() {
/*

Generated during transformation
*/
}
}
