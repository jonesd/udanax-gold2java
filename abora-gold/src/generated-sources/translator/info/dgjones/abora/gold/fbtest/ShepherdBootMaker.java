/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.fbtest;

import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.fbtest.ShepherdBootMaker;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ShepherdBootMaker extends BootMaker {

/*
udanax-top.st:56940:
BootMaker subclass: #ShepherdBootMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-fbtest'!
*/
/*
udanax-top.st:56944:
(ShepherdBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:56966:
ShepherdBootMaker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:56969:
(ShepherdBootMaker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ShepherdBootMaker.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Category bootCategory() {
	return AboraSupport.findCategory(Counter.class);
/*
udanax-top.st:56949:ShepherdBootMaker methodsFor: 'accessing'!
{Category} bootCategory
	^Counter!
*/
}
public Heaper bootHeaper() {
	return Counter.make();
/*
udanax-top.st:56954:ShepherdBootMaker methodsFor: 'protected:'!
{Heaper} bootHeaper
	^Counter make.!
*/
}
public ShepherdBootMaker(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:56959:ShepherdBootMaker methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:56962:ShepherdBootMaker methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static BootPlan make() {
	return new ShepherdBootMaker();
/*
udanax-top.st:56974:ShepherdBootMaker class methodsFor: 'creation'!
{BootPlan} make
	^self create!
*/
}
public ShepherdBootMaker() {
/*

Generated during transformation
*/
}
}
