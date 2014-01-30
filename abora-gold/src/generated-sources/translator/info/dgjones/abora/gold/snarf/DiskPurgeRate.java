/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskPurgeRate;
import info.dgjones.abora.gold.snarf.Purgeror;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Set the number of GCs between purges of the packer.
 */
public class DiskPurgeRate extends Thunk {

	protected int myCount;
/*
udanax-top.st:57237:
Thunk subclass: #DiskPurgeRate
	instanceVariableNames: 'myCount {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:57241:
DiskPurgeRate comment:
'Set the number of GCs between purges of the packer.'!
*/
/*
udanax-top.st:57243:
(DiskPurgeRate getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskPurgeRate.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Set the number of GCs between packer purges.
 */
public void execute() {
	Purgeror.setPurgeRate(myCount);
/*
udanax-top.st:57248:DiskPurgeRate methodsFor: 'operate'!
{void} execute
	"Set the number of GCs between packer purges."
	
	Purgeror setPurgeRate: myCount!
*/
}
public DiskPurgeRate(Rcvr receiver) {
	super(receiver);
	myCount = receiver.receiveIntegerVar();
/*
udanax-top.st:57255:DiskPurgeRate methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCount _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myCount);
/*
udanax-top.st:57259:DiskPurgeRate methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myCount.!
*/
}
public DiskPurgeRate() {
/*

Generated during transformation
*/
}
}
