/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cache;

import info.dgjones.abora.gold.cache.SuspendedHeaper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Heapers cached to avoid memory allocation overhead are kept as SuspendedHeapers to reduce
 * GC overhead.
 */
public class SuspendedHeaper extends Heaper {

/*
udanax-top.st:56449:
Heaper subclass: #SuspendedHeaper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-cache'!
*/
/*
udanax-top.st:56453:
SuspendedHeaper comment:
'Heapers cached to avoid memory allocation overhead are kept as SuspendedHeapers to reduce GC overhead.'!
*/
/*
udanax-top.st:56455:
(SuspendedHeaper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SuspendedHeaper.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public SuspendedHeaper() {
	super();
/*
udanax-top.st:56460:SuspendedHeaper methodsFor: 'creation'!
{INLINE} create
	super create!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:56465:SuspendedHeaper methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:56467:SuspendedHeaper methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public SuspendedHeaper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
