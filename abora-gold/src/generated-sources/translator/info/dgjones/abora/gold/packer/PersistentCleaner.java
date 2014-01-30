/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.packer;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.packer.PersistentCleaner;
import info.dgjones.abora.gold.schunk.ChunkCleaner;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * This does a makePersistent when ServerChunks go away
 */
public class PersistentCleaner extends ChunkCleaner {

	protected static PersistentCleaner ThePersistentCleaner;
/*
udanax-top.st:13695:
ChunkCleaner subclass: #PersistentCleaner
	instanceVariableNames: ''
	classVariableNames: 'ThePersistentCleaner {PersistentCleaner} '
	poolDictionaries: ''
	category: 'Xanadu-packer'!
*/
/*
udanax-top.st:13699:
PersistentCleaner comment:
'This does a makePersistent when ServerChunks go away'!
*/
/*
udanax-top.st:13701:
(PersistentCleaner getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:13715:
PersistentCleaner class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13718:
(PersistentCleaner getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PersistentCleaner.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void cleanup() {
	((DiskManager) CurrentPacker.fluidGet()).purge();
/*
udanax-top.st:13706:PersistentCleaner methodsFor: 'invoking'!
{void} cleanup
	CurrentPacker fluidGet purge!
*/
}
public PersistentCleaner() {
	super();
/*
udanax-top.st:13711:PersistentCleaner methodsFor: 'protected: create'!
create
	super create!
*/
}
public static void linkTimeNonInherited() {
	ThePersistentCleaner = null;
/*
udanax-top.st:13723:PersistentCleaner class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	ThePersistentCleaner := NULL!
*/
}
public static PersistentCleaner make() {
	if (ThePersistentCleaner == null) {
		ThePersistentCleaner = new PersistentCleaner();
	}
	return ThePersistentCleaner;
/*
udanax-top.st:13728:PersistentCleaner class methodsFor: 'create'!
make
	ThePersistentCleaner == NULL ifTrue: [ThePersistentCleaner := self create].
	^ ThePersistentCleaner!
*/
}
public PersistentCleaner(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
