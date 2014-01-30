/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.schunk;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.schunk.ChunkCleaner;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Chunk cleaners perform end-of-session cleanup work.  This includes making sure that
 * session level objects are released.
 */
public class ChunkCleaner extends Heaper {

	protected ChunkCleaner myNext;
	protected static ChunkCleaner FirstCleaner;
/*
udanax-top.st:13642:
Heaper subclass: #ChunkCleaner
	instanceVariableNames: 'myNext {ChunkCleaner}'
	classVariableNames: 'FirstCleaner {ChunkCleaner} '
	poolDictionaries: ''
	category: 'Xanadu-schunk'!
*/
/*
udanax-top.st:13646:
ChunkCleaner comment:
'Chunk cleaners perform end-of-session cleanup work.  This includes making sure that session level objects are released.'!
*/
/*
udanax-top.st:13648:
(ChunkCleaner getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:13675:
ChunkCleaner class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13678:
(ChunkCleaner getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ChunkCleaner.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public ChunkCleaner next() {
	return myNext;
/*
udanax-top.st:13653:ChunkCleaner methodsFor: 'private: accessing'!
{ChunkCleaner} next
	^ myNext!
*/
}
public void cleanup() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:13658:ChunkCleaner methodsFor: 'invoking'!
{void} cleanup
	self subclassResponsibility!
*/
}
public ChunkCleaner() {
	super();
	myNext = FirstCleaner;
	FirstCleaner = this;
/*
udanax-top.st:13663:ChunkCleaner methodsFor: 'protected: create'!
create
	super create.
	myNext := FirstCleaner.
	FirstCleaner := self.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:13670:ChunkCleaner methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:13672:ChunkCleaner methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void beClean() {
	ChunkCleaner cleaner;
	cleaner = FirstCleaner;
	while (cleaner != null) {
		cleaner.cleanup();
		cleaner = cleaner.next();
	}
/*
udanax-top.st:13683:ChunkCleaner class methodsFor: 'cleanup'!
{void} beClean
	| cleaner {ChunkCleaner} |
	cleaner := FirstCleaner.
	[cleaner ~~ NULL] whileTrue: [
		cleaner cleanup.
		cleaner := cleaner next].!
*/
}
public static void linkTimeNonInherited() {
	FirstCleaner = null;
/*
udanax-top.st:13692:ChunkCleaner class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	FirstCleaner := NULL!
*/
}
public ChunkCleaner(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
