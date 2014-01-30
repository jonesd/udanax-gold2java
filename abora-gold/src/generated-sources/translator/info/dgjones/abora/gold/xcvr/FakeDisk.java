/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.FakePacker;
import info.dgjones.abora.gold.snarf.MockTurtle;
import info.dgjones.abora.gold.xcvr.FakeDisk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;

public class FakeDisk extends Thunk {

	protected Category myCategory;
/*
udanax-top.st:57293:
Thunk subclass: #FakeDisk
	instanceVariableNames: 'myCategory {Category}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:57297:
(FakeDisk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FakeDisk.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	FakePacker.make();
	MockTurtle.make(myCategory);
/*
udanax-top.st:57302:FakeDisk methodsFor: 'running'!
{void} execute
	FakePacker make.
	MockTurtle make: myCategory.!
*/
}
public FakeDisk(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
/*
udanax-top.st:57308:FakeDisk methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCategory _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myCategory);
/*
udanax-top.st:57312:FakeDisk methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.!
*/
}
public FakeDisk() {
/*

Generated during transformation
*/
}
}
