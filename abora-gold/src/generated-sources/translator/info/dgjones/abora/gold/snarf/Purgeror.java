/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.gchooks.SanitationEngineer;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.Purgeror;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * We are about to garbage collect.  Every so often, purge the objects that are clean so
 * their flocks can be garbage collected.
 */
public class Purgeror extends SanitationEngineer {

	protected int myCount;
	protected DiskManager myPacker;
	protected boolean myPurgePending;
	protected static int PurgeRate;
/*
udanax-top.st:44966:
SanitationEngineer subclass: #Purgeror
	instanceVariableNames: '
		myCount {IntegerVar}
		myPacker {DiskManager}
		myPurgePending {BooleanVar}'
	classVariableNames: 'PurgeRate {IntegerVar} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:44973:
Purgeror comment:
'We are about to garbage collect.  Every so often, purge the objects that are clean so their flocks can be garbage collected.'!
*/
/*
udanax-top.st:44975:
(Purgeror getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:45013:
Purgeror class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:45016:
(Purgeror getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Purgeror.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void clearPurgePending() {
	myPurgePending = false;
	myCount = 0;
/*
udanax-top.st:44980:Purgeror methodsFor: 'accessing'!
{void INLINE} clearPurgePending
	myPurgePending := false.
	myCount := IntegerVar0!
*/
}
public boolean purgePending() {
	return myPurgePending;
/*
udanax-top.st:44984:Purgeror methodsFor: 'accessing'!
{BooleanVar INLINE} purgePending
	^ myPurgePending!
*/
}
public Purgeror(DiskManager packer) {
	super();
	myPacker = packer;
	myCount = 0;
	myPurgePending = false;
/*
udanax-top.st:44989:Purgeror methodsFor: 'protected: creation'!
create: packer {DiskManager}
	super create.
	myPacker _ packer.
	myCount _ IntegerVar0.
	myPurgePending _ false!
*/
}
public void recycle(boolean required) {
	if (required) {
		myPurgePending = true;
		return ;
	}
	if (myCount >= PurgeRate && (PurgeRate > 0)) {
		if ( ! (((Boolean) InsideTransactionFlag.fluidFetch()).booleanValue() || (myPacker.insideCommit()))) {
			myPacker.purgeClean();
			myCount = 0;
			myPurgePending = false;
		}
		else {
			myPurgePending = true;
		}
	}
	else {
		myCount = myCount + 1;
	}
/*
udanax-top.st:44997:Purgeror methodsFor: 'invoking'!
{void} recycle: required {BooleanVar}
	required ifTrue: [
		myPurgePending := true.
		^VOID].
	(myCount >= PurgeRate and: [PurgeRate > IntegerVarZero])
		ifTrue:
			[(InsideTransactionFlag fluidFetch or: [myPacker insideCommit])
				ifFalse:
					[myPacker purgeClean.
					myCount _ IntegerVarZero.
					myPurgePending _ false]
				ifTrue:
					[myPurgePending _ true]]
		ifFalse: [myCount _ myCount + 1]!
*/
}
public static void linkTimeNonInherited() {
	PurgeRate = 40;
/*
udanax-top.st:45021:Purgeror class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	PurgeRate _ 40.
	[Abraham] USES!
*/
}
public static Purgeror make(DiskManager packer) {
	return new Purgeror(packer);
/*
udanax-top.st:45027:Purgeror class methodsFor: 'creation'!
make: packer {DiskManager}
	^self create: packer!
*/
}
public static void setPurgeRate(int count) {
	PurgeRate = count;
/*
udanax-top.st:45032:Purgeror class methodsFor: 'setting'!
{void} setPurgeRate: count {IntegerVar}
	PurgeRate _ count!
*/
}
public Purgeror() {
/*

Generated during transformation
*/
}
public Purgeror(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
