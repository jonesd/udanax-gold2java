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
import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.fbtest.BackendBootMaker;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class BackendBootMaker extends BootMaker {

/*
udanax-top.st:56870:
BootMaker subclass: #BackendBootMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fbtest'!
*/
/*
udanax-top.st:56874:
(BackendBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:56897:
BackendBootMaker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:56900:
(BackendBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BackendBootMaker.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return AboraSupport.findCategory(BeGrandMap.class);
/*
udanax-top.st:56879:BackendBootMaker methodsFor: 'accessing'!
{Category} bootCategory
	^BeGrandMap!
*/
}
public Heaper bootHeaper() {
	return BeGrandMap.make();
/*
udanax-top.st:56884:BackendBootMaker methodsFor: 'protected:'!
{Heaper} bootHeaper
	^BeGrandMap make!
*/
}
public BackendBootMaker(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:56890:BackendBootMaker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:56893:BackendBootMaker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static BootPlan make() {
	return new BackendBootMaker();
/*
udanax-top.st:56905:BackendBootMaker class methodsFor: 'creation'!
{BootPlan} make
	^self create!
*/
}
public static void initTimeNonInherited() {
	Cookbook.declareCookbook("disk", AboraSupport.findCategory(BeGrandMap.class), Smalltalk.associationAt(DISK_CUISINE).refValue(), Smalltalk.associationAt(XPP_CUISINE).refValue(), Smalltalk.associationAt(FEBE_CUISINE).refValue());
/*
udanax-top.st:56910:BackendBootMaker class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	[FeServer, Recipe] USES.
	Cookbook declareCookbook: 'disk' with: BeGrandMap with: DiskCuisine with: XppCuisine with: FebeCuisine!
*/
}
public BackendBootMaker() {
/*

Generated during transformation
*/
}
}
