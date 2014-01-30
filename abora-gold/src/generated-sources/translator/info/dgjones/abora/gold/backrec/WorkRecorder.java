/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.backrec;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.backrec.WorkRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.turtle.RecorderTrigger;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents the a persistent works or rangeWorks query
 */
public class WorkRecorder extends ResultRecorder {

/*
udanax-top.st:44697:
ResultRecorder subclass: #WorkRecorder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44701:
WorkRecorder comment:
'Represents the a persistent works or rangeWorks query'!
*/
/*
udanax-top.st:44703:
(WorkRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorkRecorder.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public WorkRecorder(Filter endorsementsFilter, TrailBlazer trailBlazer) {
	super(endorsementsFilter, ((CrossRegion) endorsementsFilter.relevantRegion()), trailBlazer);
/*
udanax-top.st:44708:WorkRecorder methodsFor: 'create'!
create: endorsementsFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: endorsementsFilter
		with: (endorsementsFilter relevantRegion cast: CrossRegion)
		with: trailBlazer!
*/
}
public boolean accepts(BeRangeElement element) {
	return element instanceof BeWork;
/*
udanax-top.st:44717:WorkRecorder methodsFor: 'accessing'!
{BooleanVar} accepts: element {BeRangeElement}
	^element isKindOf: BeWork!
*/
}
public boolean isDirectOnly() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44721:WorkRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	
	self subclassResponsibility!
*/
}
public void delayedStoreBackfollow(BeEdition edition, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44727:WorkRecorder methodsFor: 'backfollow'!
{void} delayedStoreBackfollow: edition {BeEdition}
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	
	self subclassResponsibility!
*/
}
/**
 * If there are any Works directly on the RangeElement which pass the filters, record them
 */
public void recordImmediateWorks(BeRangeElement element, RecorderFossil fossil) {
	if (element instanceof BeEdition) {
		BeEdition edition = (BeEdition) element;
		Stepper stomper = edition.currentWorks().stepper();
		for (; stomper.hasValue(); stomper.step()) {
			BeWork work = (BeWork) stomper.fetch();
			if (work == null) {
				continue ;
			}
			if ((work.canBeReadBy(keyMaster())) && (endorsementsFilter().match(work.endorsements()))) {
				(RecorderTrigger.make(fossil, work)).schedule();
			}
		}
		stomper.destroy();
	}
/*
udanax-top.st:44734:WorkRecorder methodsFor: 'backfollow'!
{void} recordImmediateWorks: element {BeRangeElement} with: fossil {RecorderFossil}
	"If there are any Works directly on the RangeElement which pass the filters, record them"
	
	element cast: BeEdition into: [ :edition |
		edition currentWorks stepper forEach: [ :work {BeWork} |
			((work canBeReadBy: self keyMaster)
					and: [self endorsementsFilter match: work endorsements])
				ifTrue:
					[(RecorderTrigger make: fossil with: work) schedule]]]
	others:
		[]!
*/
}
public WorkRecorder() {
/*

Generated during transformation
*/
}
public WorkRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
