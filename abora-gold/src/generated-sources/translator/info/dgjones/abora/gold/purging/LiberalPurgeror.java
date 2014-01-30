/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.purging;

import info.dgjones.abora.gold.gchooks.RepairEngineer;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.purging.LiberalPurgeror;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class LiberalPurgeror extends RepairEngineer {

	protected boolean myMustPurge;
	protected SnarfPacker myPacker;
/*
udanax-top.st:42177:
RepairEngineer subclass: #LiberalPurgeror
	instanceVariableNames: '
		myMustPurge {BooleanVar}
		myPacker {SnarfPacker}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-purging'!
*/
/*
udanax-top.st:42183:
(LiberalPurgeror getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:42206:
LiberalPurgeror class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:42209:
(LiberalPurgeror getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(LiberalPurgeror.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public LiberalPurgeror(SnarfPacker packer) {
	super();
	myPacker = packer;
	myMustPurge = false;
/*
udanax-top.st:42188:LiberalPurgeror methodsFor: 'protected: create'!
create: packer {SnarfPacker}
	super create.
	myPacker := packer.
	myMustPurge := false!
*/
}
public void setMustPurge() {
	myMustPurge = true;
/*
udanax-top.st:42195:LiberalPurgeror methodsFor: 'accessing'!
{void} setMustPurge
	myMustPurge := true!
*/
}
public void repair() {
	if (myMustPurge) {
		myPacker.purgeClean(true);
		myMustPurge = false;
	}
/*
udanax-top.st:42200:LiberalPurgeror methodsFor: 'invoking'!
{void} repair
	myMustPurge ifTrue:
		[myPacker purgeClean: true.
		myMustPurge := false]!
*/
}
public static LiberalPurgeror make(SnarfPacker packer) {
	return new LiberalPurgeror(packer);
/*
udanax-top.st:42214:LiberalPurgeror class methodsFor: 'create'!
make: packer {SnarfPacker}
	^ self create: packer!
*/
}
public LiberalPurgeror() {
/*

Generated during transformation
*/
}
public LiberalPurgeror(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
