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
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.turtle.RecorderTrigger;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents the a persistent transcluders or rangeTranscluders query
 */
public class EditionRecorder extends ResultRecorder {

	protected Filter myDirectFilter;
	protected Filter myIndirectFilter;
/*
udanax-top.st:44581:
ResultRecorder subclass: #EditionRecorder
	instanceVariableNames: '
		myDirectFilter {Filter}
		myIndirectFilter {Filter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44587:
EditionRecorder comment:
'Represents the a persistent transcluders or rangeTranscluders query'!
*/
/*
udanax-top.st:44589:
(EditionRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EditionRecorder.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public boolean accepts(BeRangeElement element) {
	return element instanceof BeEdition;
/*
udanax-top.st:44594:EditionRecorder methodsFor: 'accessing'!
{BooleanVar} accepts: element {BeRangeElement}
	^element isKindOf: BeEdition!
*/
}
public Filter directFilter() {
	return myDirectFilter;
/*
udanax-top.st:44598:EditionRecorder methodsFor: 'accessing'!
{Filter} directFilter
	^myDirectFilter!
*/
}
public Filter indirectFilter() {
	return myIndirectFilter;
/*
udanax-top.st:44602:EditionRecorder methodsFor: 'accessing'!
{Filter} indirectFilter
	^myIndirectFilter!
*/
}
public boolean isDirectOnly() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44606:EditionRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	
	self subclassResponsibility!
*/
}
public EditionRecorder(Filter directFilter, Filter indirectFilter, TrailBlazer trailBlazer) {
	super(((Filter) (directFilter.unionWith(indirectFilter))), ((CrossRegion) (directFilter.relevantRegion().unionWith(indirectFilter.relevantRegion()))), trailBlazer);
	myDirectFilter = directFilter;
	myIndirectFilter = indirectFilter;
/*
udanax-top.st:44612:EditionRecorder methodsFor: 'create'!
create: directFilter {Filter}
	with: indirectFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: ((directFilter unionWith: indirectFilter) cast: Filter)
		with: ((directFilter relevantRegion unionWith: indirectFilter relevantRegion ) cast: CrossRegion)
		with: trailBlazer.
	
	myDirectFilter := directFilter.
	myIndirectFilter := indirectFilter.!
*/
}
public void delayedStoreBackfollow(BeEdition edition, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	Object currentKeyMasterOldValue = AboraBlockSupport.enterFluidBindDuring(CurrentKeyMaster, keyMaster());
	try {
		if ((myDirectFilter.match(edition.visibleEndorsements())) && (edition.anyPasses((PropFinder.backfollowFinder(permissionsFilter(), myIndirectFilter))))) {
			(RecorderTrigger.make(fossil, edition)).schedule();
		}
	}
	finally {
		AboraBlockSupport.exitFluidBindDuring(CurrentKeyMaster, currentKeyMasterOldValue);
	}
/*
udanax-top.st:44625:EditionRecorder methodsFor: 'backfollow'!
{void} delayedStoreBackfollow: edition {BeEdition}
	with: finder {PropFinder unused} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum unused}
	CurrentKeyMaster fluidBind: self keyMaster during:
		[((myDirectFilter match: edition visibleEndorsements)
				and: [edition anyPasses: (PropFinder
					backfollowFinder: self permissionsFilter with: myIndirectFilter)])
			ifTrue:
				[(RecorderTrigger make: fossil with: edition) schedule]]!
*/
}
public EditionRecorder() {
/*

Generated during transformation
*/
}
public EditionRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
