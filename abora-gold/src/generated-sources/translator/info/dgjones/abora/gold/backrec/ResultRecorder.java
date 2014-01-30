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
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.canopy.AbstractRecorderFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Represents the persistent embodiment of a query operation. Can be stored on disk in the
 * form of a RecorderFossil. The abstract protocol deals with:
 * - caching previous results to avoid duplication
 * - storing results in a trail at unique positions
 * - managing persistent permissions
 * - looking for immediate results
 * - checking whether a good candidate (identified by the canopy props) should really go into
 * the trail
 */
public class ResultRecorder extends Heaper {

	protected Filter myPermissionsFilter;
	protected Filter myEndorsementsFilter;
	protected CrossRegion myRelevantEndorsements;
	protected FeKeyMaster myKeyMaster;
	protected TrailBlazer myTrailBlazer;
/*
udanax-top.st:44462:
Heaper subclass: #ResultRecorder
	instanceVariableNames: '
		myPermissionsFilter {Filter}
		myEndorsementsFilter {Filter}
		myRelevantEndorsements {CrossRegion}
		myKeyMaster {FeKeyMaster}
		myTrailBlazer {TrailBlazer}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44471:
ResultRecorder comment:
'Represents the persistent embodiment of a query operation. Can be stored on disk in the form of a RecorderFossil. The abstract protocol deals with:
	- caching previous results to avoid duplication
	- storing results in a trail at unique positions
	- managing persistent permissions
	- looking for immediate results
	- checking whether a good candidate (identified by the canopy props) should really go into the trail'!
*/
/*
udanax-top.st:44478:
(ResultRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ResultRecorder.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Whether this recorder accepts this kind of RangeElement
 */
public boolean accepts(BeRangeElement element) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44483:ResultRecorder methodsFor: 'accessing'!
{BooleanVar} accepts: element {BeRangeElement}
	"Whether this recorder accepts this kind of RangeElement"
	
	self subclassResponsibility!
*/
}
public IDRegion actualAuthority() {
	return myKeyMaster.actualAuthority();
/*
udanax-top.st:44488:ResultRecorder methodsFor: 'accessing'!
{IDRegion} actualAuthority
	^myKeyMaster actualAuthority!
*/
}
/**
 * Something to find potential candidates given a source for the query
 */
public PropFinder bertPropFinder() {
	return PropFinder.backfollowFinder(permissionsFilter(), endorsementsFilter());
/*
udanax-top.st:44492:ResultRecorder methodsFor: 'accessing'!
{PropFinder} bertPropFinder
	"Something to find potential candidates given a source for the query"
	^PropFinder 
		backfollowFinder: self permissionsFilter 
		with: self endorsementsFilter!
*/
}
/**
 * The endorsements I am looking for
 */
public Filter endorsementsFilter() {
	return myEndorsementsFilter;
/*
udanax-top.st:44499:ResultRecorder methodsFor: 'accessing'!
{Filter} endorsementsFilter  
	"The endorsements I am looking for"
	
	^myEndorsementsFilter!
*/
}
/**
 * Whether the recorder is for a query with the directContainersOnly flag
 */
public boolean isDirectOnly() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44504:ResultRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	"Whether the recorder is for a query with the directContainersOnly flag"
	
	self subclassResponsibility!
*/
}
public FeKeyMaster keyMaster() {
	return myKeyMaster;
/*
udanax-top.st:44509:ResultRecorder methodsFor: 'accessing'!
{FeKeyMaster} keyMaster
	^myKeyMaster!
*/
}
/**
 * The permissions I am looking for
 */
public Filter permissionsFilter() {
	return myPermissionsFilter;
/*
udanax-top.st:44513:ResultRecorder methodsFor: 'accessing'!
{Filter of: ID} permissionsFilter
	"The permissions I am looking for"
	
	^myPermissionsFilter!
*/
}
/**
 * A SensorProp which corresponds to what I am looking for
 */
public SensorProp sensorProp() {
	return SensorProp.make(((IDRegion) permissionsFilter().relevantRegion()), myRelevantEndorsements, false);
/*
udanax-top.st:44518:ResultRecorder methodsFor: 'accessing'!
{SensorProp} sensorProp
	"A SensorProp which corresponds to what I am looking for"
	
	^SensorProp
		make: (self permissionsFilter relevantRegion cast: IDRegion)
		with: myRelevantEndorsements
		with: false!
*/
}
/**
 * tell my TrailBlazer to recorder it
 */
public void record(BeRangeElement answer) {
	myTrailBlazer.record(answer);
/*
udanax-top.st:44528:ResultRecorder methodsFor: 'recording'!
{void} record: answer {BeRangeElement}
	"tell my TrailBlazer to recorder it"
	
	myTrailBlazer record: answer!
*/
}
/**
 * Trigger myself if I match the finder's profile
 */
public void triggerIfMatching(PropFinder finder, RecorderFossil fossil) {
	if (finder instanceof AbstractRecorderFinder) {
		AbstractRecorderFinder arf = (AbstractRecorderFinder) finder;
		arf.checkRecorder(this, fossil);
	}
/*
udanax-top.st:44533:ResultRecorder methodsFor: 'recording'!
{void} triggerIfMatching: finder {PropFinder} with: fossil {RecorderFossil}
	"Trigger myself if I match the finder's profile"
	
	finder cast: AbstractRecorderFinder into: [ :arf |
		arf checkRecorder: self with: fossil]!
*/
}
public ResultRecorder(Filter endorsementsFilter, CrossRegion relevantEndorsements, TrailBlazer trailBlazer) {
	super();
	Ravi.thingToDo();
	/* decide whether this should have a filter or just the relevant regions */
	myEndorsementsFilter = endorsementsFilter;
	myRelevantEndorsements = relevantEndorsements;
	myKeyMaster = ((FeKeyMaster) CurrentKeyMaster.fluidGet());
	myPermissionsFilter = ((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDFilterSpace().anyFilter(myKeyMaster.actualAuthority());
	myTrailBlazer = trailBlazer;
/*
udanax-top.st:44541:ResultRecorder methodsFor: 'create'!
create: endorsementsFilter {Filter}
	with: relevantEndorsements {CrossRegion}
	with: trailBlazer {TrailBlazer}
	
	super create.
	Ravi thingToDo. "decide whether this should have a filter or just the relevant regions"
	myEndorsementsFilter := endorsementsFilter.
	myRelevantEndorsements := relevantEndorsements.
	myKeyMaster := CurrentKeyMaster fluidGet.
	
	[BeGrandMap] USES.
	myPermissionsFilter := CurrentGrandMap fluidGet globalIDFilterSpace
		anyFilter: myKeyMaster actualAuthority.
	myTrailBlazer := trailBlazer!
*/
}
/**
 * The immediate part of the backfollow has reached an Edition while traversing northwards. I
 * now get to decide what to do next.
 */
public void delayedStoreBackfollow(BeEdition edition, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:44558:ResultRecorder methodsFor: 'backfollow'!
{void} delayedStoreBackfollow: edition {BeEdition}
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	"The immediate part of the backfollow has reached an Edition while traversing northwards. I now get to decide what to do next."
	
	self subclassResponsibility!
*/
}
/**
 * The immediate part of the backfollow has reached an RangeElement of the original Edition.
 * I now get to decide what to do next to continue the operation
 */
public void delayedStoreMatching(BeRangeElement element, PropFinder finder, RecorderFossil fossil, HashSetCache hCrumCache) {
	element.delayedStoreBackfollow(finder, fossil, this, hCrumCache
	/* this is a default implementation, which subclasses may override or modify */
	);
/*
udanax-top.st:44566:ResultRecorder methodsFor: 'backfollow'!
{void} delayedStoreMatching: element {BeRangeElement}
	with: finder {PropFinder} 
	with: fossil {RecorderFossil}
	with: hCrumCache {HashSetCache of: HistoryCrum}
	"The immediate part of the backfollow has reached an RangeElement of the original Edition. I now get to decide what to do next to continue the operation"
	
	 element delayedStoreBackfollow: finder with: fossil with: self with: hCrumCache
	 "this is a default implementation, which subclasses may override or modify"!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:44577:ResultRecorder methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:44579:ResultRecorder methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public ResultRecorder() {
/*

Generated during transformation
*/
}
public ResultRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
