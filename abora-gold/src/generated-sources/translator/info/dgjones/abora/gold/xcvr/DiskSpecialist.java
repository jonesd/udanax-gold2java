/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.cxx.classx.comm.StubRecipe;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.ShepherdStub;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.DiskSpecialist;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class DiskSpecialist extends TransferSpecialist {

	protected DiskManager myPacker;
	protected boolean myInsideShepherd;
	protected static InstanceCache SomeSpecialists;
/*
udanax-top.st:63206:
TransferSpecialist subclass: #DiskSpecialist
	instanceVariableNames: '
		myPacker {DiskManager}
		myInsideShepherd {BooleanVar}'
	classVariableNames: 'SomeSpecialists {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:63212:
(DiskSpecialist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:63289:
DiskSpecialist class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:63292:
(DiskSpecialist getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DiskSpecialist.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * There's a lot of smalltalk only stuff in here.  Smalltalk stubs should move towards c++
 * stubs.
 */
public Heaper receiveHeaperFrom(Category cat, SpecialistRcvr rcvr) {
	int snarfID;
	int index;
	int hash;
	Heaper result;
	Category newCat;
	if ( ! (cat.isEqualOrSubclassOf(AboraSupport.findCategory(Abraham.class)))) {
		return rcvr.basicReceive((getRecipe(cat)));
	}
	if ( ! myInsideShepherd) {
		myInsideShepherd = true;
		result = rcvr.basicReceive((getRecipe(cat)));
		myInsideShepherd = false;
		return result;
	}
	hash = rcvr.receiveUInt32();
	AboraSupport.smalltalkOnly();
	{
		newCat = rcvr.receiveCategory();
	}
	snarfID = rcvr.receiveUInt32();
	index = rcvr.receiveUInt32();
	result = myPacker.fetchCanonical(hash, snarfID, index);
	if (result == null) {
		AboraSupport.smalltalkOnly();
		{
			result = new ShepherdStub(hash, newCat);
		}
		AboraSupport.translateOnly();
		{
			result = ((StubRecipe) (getRecipe(cat))).parseStub(rcvr, hash);
		}
		if ( ! (result != null)) {
			throw new AboraAssertionException("Bad Stub");
		}
		myPacker.registerStub(((Abraham) result), snarfID, index);
	}
	rcvr.registerIbid(result);
	return result;
/*
udanax-top.st:63217:DiskSpecialist methodsFor: 'communication'!
{Heaper} receiveHeaper: cat {Category} from: rcvr {SpecialistRcvr}
 	"There's a lot of smalltalk only stuff in here.  Smalltalk stubs should move towards c++ stubs."
	| snarfID {SnarfID} index {Int32} hash {UInt32} result {Heaper} newCat {Category smalltalk} |
	(cat isEqualOrSubclassOf: Abraham) not ifTrue:
		[^rcvr basicReceive: (self getRecipe: cat)].
	myInsideShepherd not ifTrue: 
		[myInsideShepherd _ true.
		result _ rcvr basicReceive: (self getRecipe: cat).
		myInsideShepherd _ false.
		^result].
	hash _ rcvr receiveUInt32.
	[newCat _ rcvr receiveCategory] smalltalkOnly.
	snarfID _ rcvr receiveUInt32.
	index _ rcvr receiveUInt32.
	result _ myPacker fetchCanonical: hash with: snarfID with: index.
	result == NULL ifTrue: 
		[[result _ ShepherdStub create: hash with: newCat] smalltalkOnly.
		[result _ ((self getRecipe: cat) cast: StubRecipe) parseStub: rcvr with: hash] translateOnly.
		result ~~ NULL assert: 'Bad Stub'.
		myPacker registerStub: (result cast: Abraham) with: snarfID with: index].
	rcvr registerIbid: result.
	^result!
*/
}
/**
 * Return an object from the rcvr or NULL if cat is not a category that we
 * handle specially.
 */
public void receiveHeaperIntoFrom(Category cat, Heaper memory, SpecialistRcvr rcvr) {
	if (cat.isEqualOrSubclassOf(AboraSupport.findCategory(Abraham.class))) {
		if ((getRecipe(cat)).isKindOf(AboraSupport.findCategory(StubRecipe.class))) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_BECOMABLE);
		}
		else {
			if ( ! myInsideShepherd) {
				myInsideShepherd = true;
				rcvr.basicReceiveInto((getRecipe(cat)), memory);
				myInsideShepherd = false;
				return ;
			}
		}
	}
	rcvr.basicReceiveInto((getRecipe(cat)), memory);
/*
udanax-top.st:63241:DiskSpecialist methodsFor: 'communication'!
{void} receiveHeaper: cat {Category} into: memory {Heaper} from: rcvr {SpecialistRcvr} 
	"Return an object from the rcvr or NULL if cat is not a category that we 
	handle specially."
	(cat isEqualOrSubclassOf: Abraham)
		ifTrue: [((self getRecipe: cat) isKindOf: StubRecipe)
				ifTrue: [Heaper BLAST: #NotBecomable]
				ifFalse: [myInsideShepherd not
						ifTrue: 
							[myInsideShepherd _ true.
							rcvr basicReceive: (self getRecipe: cat) into: memory.
							myInsideShepherd _ false.
							^VOID]]].
	rcvr basicReceive: (self getRecipe: cat) into: memory!
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
			xmtr.sendUInt32(abe.getInfo().snarfID());
			xmtr.sendUInt32(abe.getInfo().index());
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
udanax-top.st:63255:DiskSpecialist methodsFor: 'communication'!
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
					xmtr sendUInt32: abe getInfo snarfID.
					xmtr sendUInt32: abe getInfo index.
					xmtr endInstance]
				ifFalse: 
					[myInsideShepherd _ true.
					super sendHeaper: abe to: xmtr.
					myInsideShepherd _ false].
			^VOID]
		others: [super sendHeaper: hpr to: xmtr]!
*/
}
public DiskSpecialist(Cookbook cookbook, DiskManager packer) {
	super(cookbook);
	myPacker = packer;
	myInsideShepherd = false;
/*
udanax-top.st:63280:DiskSpecialist methodsFor: 'create'!
create: cookbook {Cookbook} with: packer {DiskManager}
	super create: cookbook.
	myPacker _ packer.
	myInsideShepherd _ false!
*/
}
public void destroy() {
	if ( ! (SomeSpecialists.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:63285:DiskSpecialist methodsFor: 'create'!
{void} destroy
	(SomeSpecialists store: self) ifFalse: [super destroy]!
*/
}
public static void initTimeNonInherited() {
	SomeSpecialists = InstanceCache.make(16);
/*
udanax-top.st:63297:DiskSpecialist class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeSpecialists := InstanceCache make: 16!
*/
}
public static void linkTimeNonInherited() {
	SomeSpecialists = null;
/*
udanax-top.st:63300:DiskSpecialist class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeSpecialists := NULL!
*/
}
public static TransferSpecialist make(Cookbook book, DiskManager packer) {
	Heaper result;
	result = SomeSpecialists.fetch();
	if (result == null) {
		return new DiskSpecialist(book, packer);
	}
	else {
		return 
		/* TODO newBecome */
		new DiskSpecialist(book, packer);
	}
/*
udanax-top.st:63305:DiskSpecialist class methodsFor: 'stream creation'!
{TransferSpecialist} make: book {Cookbook} with: packer {DiskManager}
	| result {Heaper} |
	result := SomeSpecialists fetch.
	result == NULL
		ifTrue: [^self create: book with: packer]
		ifFalse: [^(self new.Become: result) create: book with: packer]!
*/
}
public DiskSpecialist() {
/*

Generated during transformation
*/
}
public DiskSpecialist(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
