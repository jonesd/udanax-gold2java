/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.backrec;

import info.dgjones.abora.gold.backrec.EditionRecorder;
import info.dgjones.abora.gold.backrec.IndirectEditionRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents the a persistent transcluders or rangeTranscluders query with
 * directContainersOnly flag off
 */
public class IndirectEditionRecorder extends EditionRecorder {

/*
udanax-top.st:44662:
EditionRecorder subclass: #IndirectEditionRecorder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44666:
IndirectEditionRecorder comment:
'Represents the a persistent transcluders or rangeTranscluders query with directContainersOnly flag off'!
*/
/*
udanax-top.st:44668:
(IndirectEditionRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IndirectEditionRecorder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean isDirectOnly() {
	return false;
/*
udanax-top.st:44673:IndirectEditionRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	
	^false!
*/
}
public IndirectEditionRecorder(Filter directFilter, Filter indirectFilter, TrailBlazer trailBlazer) {
	super(directFilter, indirectFilter, trailBlazer);
/*
udanax-top.st:44679:IndirectEditionRecorder methodsFor: 'create'!
create: directFilter {Filter}
	with: indirectFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: directFilter
		with: indirectFilter
		with: trailBlazer!
*/
}
public void delayedStoreBackfollow(BeEdition edition, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	super.delayedStoreBackfollow(edition, finder, fossil, hCrumCache);
	edition.delayedStoreBackfollow(finder, fossil, this, hCrumCache);
/*
udanax-top.st:44689:IndirectEditionRecorder methodsFor: 'backfollow'!
{void} delayedStoreBackfollow: edition {BeEdition}
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	super delayedStoreBackfollow: edition with: finder with: fossil with: hCrumCache.
	edition delayedStoreBackfollow: finder with: fossil with: self with: hCrumCache.!
*/
}
public IndirectEditionRecorder() {
/*

Generated during transformation
*/
}
public IndirectEditionRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
