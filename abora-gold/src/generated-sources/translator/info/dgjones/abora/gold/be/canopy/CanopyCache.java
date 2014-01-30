/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.be.canopy.CanopyCache;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class CanopyCache extends Heaper {

	protected CanopyCrum myCachedCrum;
	protected CanopyCrum myCachedRoot;
	protected MuSet myCachedPath;
/*
udanax-top.st:13096:
Heaper subclass: #CanopyCache
	instanceVariableNames: '
		myCachedCrum {CanopyCrum}
		myCachedRoot {CanopyCrum}
		myCachedPath {MuSet of: CanopyCrum}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:13103:
(CanopyCache getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:13173:
CanopyCache class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13176:
(CanopyCache getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CanopyCache.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CanopyCache() {
	super();
	myCachedCrum = null;
	myCachedRoot = null;
	myCachedPath = MuSet.make();
/*
udanax-top.st:13108:CanopyCache methodsFor: 'protected: creation'!
create
	super create.
	myCachedCrum _ NULL.
	myCachedRoot _ NULL.
	myCachedPath _ MuSet make!
*/
}
/**
 * Clear the cache because the canopy has
 * changed.  This ought to destroy the cachedPath.
 * This must be cleared after every episode!!!!!!
 */
public void clearCache() {
	myCachedCrum = null;
	myCachedRoot = null;
	myCachedPath = MuSet.make();
/*
udanax-top.st:13116:CanopyCache methodsFor: 'operations'!
{void} clearCache
	"Clear the cache because the canopy has
	 changed.  This ought to destroy the cachedPath. 
	 This must be cleared after every episode!!!!!!"
	myCachedCrum _ NULL.
	myCachedRoot _ NULL.
	myCachedPath _ MuSet make.!
*/
}
/**
 * Return the set of all crums from canopyCrum
 * (inclusive) to the top of canopyCrum's canopy.
 */
public MuSet pathFor(CanopyCrum canopyCrum) {
	if ( ! ((myCachedCrum) == canopyCrum)) {
		CanopyCrum cur;
		cur = canopyCrum;
		myCachedCrum = canopyCrum;
		myCachedRoot = canopyCrum;
		myCachedPath = MuSet.make();
		while (cur != null) {
			myCachedRoot = cur;
			myCachedPath.store(cur);
			cur = cur.fetchParent();
		}
	}
	return myCachedPath;
/*
udanax-top.st:13125:CanopyCache methodsFor: 'operations'!
{MuSet of: CanopyCrum} pathFor: canopyCrum {CanopyCrum} 
	"Return the set of all crums from canopyCrum 
	(inclusive) to the top of canopyCrum's canopy."
	(myCachedCrum basicCast: Heaper star) == canopyCrum
		ifFalse: 
			[| cur {CanopyCrum} |
			cur _ canopyCrum.
			myCachedCrum _ canopyCrum.
			myCachedRoot _ canopyCrum.
			myCachedPath _ MuSet make.
			[cur ~~ NULL]
				whileTrue: 
					[myCachedRoot _  cur.
					myCachedPath store: cur.
					cur _ cur fetchParent]].
	^myCachedPath!
*/
}
/**
 * Return the crum at the top of canopyCrum's canopy.
 */
public CanopyCrum rootFor(CanopyCrum bertCrum) {
	pathFor(bertCrum);
	return myCachedRoot;
/*
udanax-top.st:13143:CanopyCache methodsFor: 'operations'!
{CanopyCrum} rootFor: bertCrum {CanopyCrum} 
	"Return the crum at the top of canopyCrum's canopy."
	self pathFor: bertCrum.
	^myCachedRoot!
*/
}
/**
 * If the cache contains childCrum it must be made
 * to contain childCrum's new parent: parentCrum.
 * Also update CachedRoot.
 */
public void updateCacheForParent(CanopyCrum childCrum, CanopyCrum parentCrum) {
	if (myCachedPath.hasMember(childCrum)) {
		myCachedPath.store(parentCrum);
		if ((myCachedRoot) == childCrum) {
			myCachedRoot = parentCrum;
		}
	}
/*
udanax-top.st:13149:CanopyCache methodsFor: 'operations'!
{void} updateCache: childCrum {CanopyCrum} forParent: parentCrum {CanopyCrum} 
	"If the cache contains childCrum it must be made 
	to contain childCrum's new parent: parentCrum. 
	Also update CachedRoot." 
	(myCachedPath hasMember: childCrum) ifTrue: 
		[myCachedPath store: parentCrum.
		 (myCachedRoot basicCast: Heaper star) == childCrum 
			ifTrue: [myCachedRoot _ parentCrum]]!
*/
}
/**
 * If the cache contains canopyCrum, it must be updated
 * because canopyCrum has new parents. For now, just
 * invalidate the cache.
 */
public void updateCacheFor(CanopyCrum canopyCrum) {
	if ((myCachedCrum) == canopyCrum) {
		clearCache();
	}
/*
udanax-top.st:13159:CanopyCache methodsFor: 'operations'!
{void} updateCacheFor: canopyCrum {CanopyCrum} 
	"If the cache contains canopyCrum, it must be updated 
	because canopyCrum has new parents. For now, just 
	invalidate the cache." 
	(myCachedCrum basicCast: Heaper star) == canopyCrum
		ifTrue: [self clearCache]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:13169:CanopyCache methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static CanopyCache make() {
	return new CanopyCache();
/*
udanax-top.st:13181:CanopyCache class methodsFor: 'make'!
make
	^ self create!
*/
}
public CanopyCache(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
