/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskCountSpecialist;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class DiskCountSpecialist extends TransferSpecialist {

	protected boolean myInsideShepherd;
	protected static int MaxFlocks;
	protected static int MaxSnarfs;
	protected static InstanceCache SomeSpecialists;
/*
udanax-top.st:63125:
TransferSpecialist subclass: #DiskCountSpecialist
	instanceVariableNames: 'myInsideShepherd {BooleanVar}'
	classVariableNames: '
		MaxFlocks {Int32} 
		MaxSnarfs {Int32} 
		SomeSpecialists {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:63132:
(DiskCountSpecialist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:63181:
DiskCountSpecialist class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63184:
(DiskCountSpecialist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskCountSpecialist.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public DiskCountSpecialist(Cookbook cookbook) {
	super(cookbook);
	myInsideShepherd = false;
/*
udanax-top.st:63137:DiskCountSpecialist methodsFor: 'creation'!
create: cookbook {Cookbook}
	super create: cookbook.
	myInsideShepherd _ false!
*/
}
public void destroy() {
	if ( ! (SomeSpecialists.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:63141:DiskCountSpecialist methodsFor: 'creation'!
{void} destroy
	(SomeSpecialists store: self) ifFalse: [super destroy]!
*/
}
/**
 * DiskCountSpecialist are only for sending.
 */
public Heaper receiveHeaperFrom(Category cat, SpecialistRcvr rcvr) {
	throw new AboraRuntimeException(AboraRuntimeException.INCOMPLETE_ABSTRACTION);
/*
udanax-top.st:63146:DiskCountSpecialist methodsFor: 'communication'!
{Heaper} receiveHeaper: cat {Category unused} from: rcvr {SpecialistRcvr unused}
 	"DiskCountSpecialist are only for sending."
	Heaper BLAST: #IncompleteAbstraction.
	^NULL!
*/
}
/**
 * DiskCountSpecialist are only for sending.
 */
public void receiveHeaperIntoFrom(Category cat, Heaper memory, SpecialistRcvr rcvr) {
	throw new AboraRuntimeException(AboraRuntimeException.INCOMPLETE_ABSTRACTION);
/*
udanax-top.st:63152:DiskCountSpecialist methodsFor: 'communication'!
{void} receiveHeaper: cat {Category unused} into: memory {Heaper unused} from: rcvr {SpecialistRcvr unused} 
	"DiskCountSpecialist are only for sending."
	
	Heaper BLAST: #IncompleteAbstraction!
*/
}
/**
 * Handle sending Shepherds specially.
 */
public void sendHeaperTo(Heaper hpr, SpecialistXmtr xmtr) {
	if (hpr instanceof Abraham) {
		Abraham abe = (Abraham) hpr;
		if (myInsideShepherd) {
			abe.getInfo();
			/* Test to verify that all persistently pointed-at sheps didi newShepherd. */
			xmtr.startInstance(abe, abe.getShepherdStubCategory());
			xmtr.sendUInt32(abe.hashForEqual());
			xmtr.sendCategory(((abe.isStub()) ? abe.getCategoryFromStub() : abe.getCategory()));
			xmtr.sendUInt32(MaxSnarfs);
			xmtr.sendUInt32(MaxFlocks);
			xmtr.endInstance();
		}
		else {
			myInsideShepherd = true;
			super.sendHeaperTo(abe, xmtr);
			myInsideShepherd = false;
		}
		return ;
	}
	else {
		super.sendHeaperTo(hpr, xmtr);
	}
/*
udanax-top.st:63157:DiskCountSpecialist methodsFor: 'communication'!
{void} sendHeaper: hpr {Heaper} to: xmtr {SpecialistXmtr} 
	"Handle sending Shepherds specially."
	hpr cast: Abraham into: [:abe |
			myInsideShepherd
				ifTrue: 
					[abe getInfo.  "Test to verify that all persistently pointed-at sheps didi newShepherd."
					xmtr startInstance: abe with: abe getShepherdStubCategory.
					xmtr sendUInt32: abe hashForEqual.
					[xmtr sendCategory:
						(abe isStub
							ifTrue: [abe getCategoryFromStub]
							ifFalse: [abe getCategory])] smalltalkOnly.
					xmtr sendUInt32: MaxSnarfs.
					xmtr sendUInt32: MaxFlocks.
					xmtr endInstance]
				ifFalse: 
					[myInsideShepherd _ true.
					super sendHeaper: abe to: xmtr.
					myInsideShepherd _ false].
			^VOID]
		others: [super sendHeaper: hpr to: xmtr]!
*/
}
public static TransferSpecialist make(Cookbook aBook) {
	Heaper result;
	result = SomeSpecialists.fetch();
	if (result == null) {
		return new DiskCountSpecialist(aBook);
	}
	else {
		return 
		/* TODO newBecome */
		new DiskCountSpecialist(aBook);
	}
/*
udanax-top.st:63189:DiskCountSpecialist class methodsFor: 'creation'!
{TransferSpecialist} make: aBook {Cookbook}
	| result {Heaper} |
	result := SomeSpecialists fetch.
	result == NULL
		ifTrue: [^self create: aBook]
		ifFalse: [^(self new.Become: result) create: aBook]!
*/
}
public static void initTimeNonInherited() {
	SomeSpecialists = InstanceCache.make(16);
/*
udanax-top.st:63198:DiskCountSpecialist class methodsFor: 'smalltalk: initialization'!
initTimeNonInherited
	SomeSpecialists := InstanceCache make: 16!
*/
}
public static void linkTimeNonInherited() {
	MaxSnarfs = 3000000;
	MaxFlocks = 3000000;
	SomeSpecialists = null;
/*
udanax-top.st:63201:DiskCountSpecialist class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	MaxSnarfs _ 3000000.
	MaxFlocks _ 3000000.
	SomeSpecialists := NULL!
*/
}
public DiskCountSpecialist() {
/*

Generated during transformation
*/
}
public DiskCountSpecialist(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
