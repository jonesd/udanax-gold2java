/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.backrec;

import info.dgjones.abora.gold.backrec.IndirectWorkRecorder;
import info.dgjones.abora.gold.backrec.WorkRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents the a persistent works or rangeWorks query with the directContainersOnly flag
 * off
 */
public class IndirectWorkRecorder extends WorkRecorder {

/*
udanax-top.st:44785:
WorkRecorder subclass: #IndirectWorkRecorder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44789:
IndirectWorkRecorder comment:
'Represents the a persistent works or rangeWorks query with the directContainersOnly flag off'!
*/
/*
udanax-top.st:44791:
(IndirectWorkRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IndirectWorkRecorder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public IndirectWorkRecorder(Filter endorsementsFilter, TrailBlazer trailBlazer) {
	super(endorsementsFilter, trailBlazer);
/*
udanax-top.st:44796:IndirectWorkRecorder methodsFor: 'create'!
create: endorsementsFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: endorsementsFilter
		with: trailBlazer!
*/
}
public boolean isDirectOnly() {
	return false;
/*
udanax-top.st:44804:IndirectWorkRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	
	^false!
*/
}
public void delayedStoreBackfollow(BeEdition edition, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	recordImmediateWorks(edition, fossil);
	edition.delayedStoreBackfollow(finder, fossil, this, hCrumCache);
/*
udanax-top.st:44810:IndirectWorkRecorder methodsFor: 'backfollow'!
{void} delayedStoreBackfollow: edition {BeEdition}
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	self recordImmediateWorks: edition with: fossil.
	edition delayedStoreBackfollow: finder with: fossil with: self with: hCrumCache!
*/
}
public void delayedStoreMatching(BeRangeElement element, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	recordImmediateWorks(element, fossil);
	super.delayedStoreMatching(element, finder, fossil, hCrumCache);
/*
udanax-top.st:44818:IndirectWorkRecorder methodsFor: 'backfollow'!
{void} delayedStoreMatching: element {BeRangeElement}
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	 self recordImmediateWorks: element with: fossil.
	 super delayedStoreMatching: element with: finder with: fossil with: hCrumCache!
*/
}
public IndirectWorkRecorder() {
/*

Generated during transformation
*/
}
public IndirectWorkRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
