/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.diskman;

import info.dgjones.abora.gold.diskman.HonestAbeIniter;
import info.dgjones.abora.gold.diskman.Honestly;
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

public class Honestly extends Thunk {

	protected Category myCategory;
	protected boolean blastOnError;
	protected int persistInterval;
/*
udanax-top.st:57382:
Thunk subclass: #Honestly
	instanceVariableNames: '
		myCategory {Category}
		blastOnError {BooleanVar}
		persistInterval {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-diskman'!
*/
/*
udanax-top.st:57389:
(Honestly getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Honestly.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	if (((DiskManager) CurrentPacker.fluidFetch()) == null) {
		TestPacker.make(blastOnError, persistInterval);
		Turtle.make(null, myCategory, ProtocolBroker.diskProtocol());
	}
	CurrentGrandMap.fluidSet(HonestAbeIniter.fetchGrandMap());
/*
udanax-top.st:57394:Honestly methodsFor: 'running'!
{void} execute
	CurrentPacker fluidFetch == NULL ifTrue:
		[TestPacker make: blastOnError with: persistInterval.
		Turtle make: NULL with: myCategory with: ProtocolBroker diskProtocol].
	CurrentGrandMap fluidSet: HonestAbeIniter fetchGrandMap.!
*/
}
public Honestly(Rcvr receiver) {
	super(receiver);
	myCategory = (Category) receiver.receiveHeaper();
	blastOnError = receiver.receiveBooleanVar();
	persistInterval = receiver.receiveIntegerVar();
/*
udanax-top.st:57402:Honestly methodsFor: 'generated:'!
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
udanax-top.st:57408:Honestly methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myCategory.
	xmtr sendBooleanVar: blastOnError.
	xmtr sendIntegerVar: persistInterval.!
*/
}
public Honestly() {
/*

Generated during transformation
*/
}
}
