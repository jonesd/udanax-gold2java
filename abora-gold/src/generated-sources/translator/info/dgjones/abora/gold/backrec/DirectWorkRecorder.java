/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.backrec;

import info.dgjones.abora.gold.backrec.DirectWorkRecorder;
import info.dgjones.abora.gold.backrec.WorkRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents the a persistent works or rangeWorks query with the directContainersOnly flag
 * on
 */
public class DirectWorkRecorder extends WorkRecorder {

/*
udanax-top.st:44746:
WorkRecorder subclass: #DirectWorkRecorder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44750:
DirectWorkRecorder comment:
'Represents the a persistent works or rangeWorks query with the directContainersOnly flag on'!
*/
/*
udanax-top.st:44752:
(DirectWorkRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DirectWorkRecorder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public DirectWorkRecorder(Filter endorsementsFilter, TrailBlazer trailBlazer) {
	super(endorsementsFilter, trailBlazer);
/*
udanax-top.st:44757:DirectWorkRecorder methodsFor: 'create'!
create: endorsementsFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: endorsementsFilter
		with: trailBlazer!
*/
}
public boolean isDirectOnly() {
	return true;
/*
udanax-top.st:44765:DirectWorkRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	
	^true!
*/
}
public void delayedStoreBackfollow(BeEdition edition, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
/*
udanax-top.st:44771:DirectWorkRecorder methodsFor: 'backfollow'!
{void} delayedStoreBackfollow: edition {BeEdition unused}
	with: finder {PropFinder unused} 
	with: fossil {RecorderFossil unused}
	with: hCrumCache {HashSetCache unused of: HistoryCrum }
	
	Heaper BLAST: #FatalError. "This algorithm should never reach here"!
*/
}
public void delayedStoreMatching(BeRangeElement element, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	recordImmediateWorks(element, fossil
	/* and nothing else */
	);
/*
udanax-top.st:44778:DirectWorkRecorder methodsFor: 'backfollow'!
{void} delayedStoreMatching: element {BeRangeElement}
	with: finder {PropFinder unused} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache unused of: HistoryCrum }
	
	 self recordImmediateWorks: element with: fossil "and nothing else"!
*/
}
public DirectWorkRecorder() {
/*

Generated during transformation
*/
}
public DirectWorkRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
