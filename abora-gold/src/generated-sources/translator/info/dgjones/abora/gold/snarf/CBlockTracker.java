/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.CBlockTracker;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class CBlockTracker extends Heaper {

	protected String myFileName;
	protected int myLineNo;
	protected int myMaxDirty;
	protected int myLimit;
	protected int myDirtySoFar;
	protected int myTrulyDirtySoFar;
	protected MuSet myDirtyInfos;
	protected int myDirtyInfosCount;
	protected CBlockTracker myOuterTracker;
	protected int myOccurencesCount;
	protected static CBlockTracker TheTrackerList;
/*
udanax-top.st:13220:
Heaper subclass: #CBlockTracker
	instanceVariableNames: '
		myFileName {char star | NULL}
		myLineNo {Int4}
		myMaxDirty {IntegerVar}
		myLimit {IntegerVar}
		myDirtySoFar {IntegerVar}
		myTrulyDirtySoFar {IntegerVar}
		myDirtyInfos {MuSet of: IntegerPos}
		myDirtyInfosCount {IntegerVar}
		myOuterTracker {CBlockTracker | NULL}
		myOccurencesCount {IntegerVar}'
	classVariableNames: 'TheTrackerList {CBlockTracker | NULL} '
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:13234:
(CBlockTracker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:13405:
CBlockTracker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13408:
(CBlockTracker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CBlockTracker.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CBlockTracker(int dirty, CBlockTracker outer) {
	super();
	if (dirty == -1) {
		myMaxDirty = 1000;
	}
	else {
		myMaxDirty = dirty;
	}
	myOuterTracker = outer;
	myFileName = null;
	myLineNo = 0;
	myDirtySoFar = 0;
	myTrulyDirtySoFar = 0;
	myDirtyInfos = MuSet.make();
	myDirtyInfosCount = 0;
	myOccurencesCount = 1;
	if (outer == null) {
		myLimit = myMaxDirty;
	}
	else {
		myLimit = Math.min(outer.slack(), myMaxDirty);
	}
/*
udanax-top.st:13239:CBlockTracker methodsFor: 'creation'!
create: dirty {IntegerVar} with: outer {CBlockTracker | NULL} 
	
	super create.
	dirty = -1
		ifTrue: [myMaxDirty _ 1000]
		ifFalse: [myMaxDirty _ dirty].
	myOuterTracker _ outer.
	myFileName _ NULL.
	myLineNo _ Int32Zero.
	myDirtySoFar _ Int32Zero.
	myTrulyDirtySoFar _ Int32Zero.
	myDirtyInfos _ MuSet make.
	myDirtyInfosCount _ Int32Zero.
	myOccurencesCount _ 1.
	outer == NULL
		ifTrue: [myLimit _ myMaxDirty]
		ifFalse: [myLimit _ outer slack min: myMaxDirty]!
*/
}
public void dirty(FlockInfo info) {
	myDirtySoFar = myDirtySoFar + 1;
	myTrulyDirtySoFar = myTrulyDirtySoFar + 1;
	if ( ! (info != null)) {
		throw new AboraAssertionException();
	}
	myDirtyInfos.store((IntegerPos.make(info.getShepherd().hashForEqual())));
	myDirtyInfosCount = myDirtyInfos.count();
	reportProblems();
/*
udanax-top.st:13259:CBlockTracker methodsFor: 'accessing'!
{void} dirty: info {FlockInfo | NULL} 
	
	myDirtySoFar _ myDirtySoFar + 1.
	myTrulyDirtySoFar _ myTrulyDirtySoFar + 1.
	(info ~~ NULL) assert.
	myDirtyInfos store: (IntegerPos make: info getShepherd hashForEqual).
	myDirtyInfosCount _ myDirtyInfos count.
	self reportProblems!
*/
}
public CBlockTracker fetchUnwrapped() {
	CBlockTracker result;
	CBlockTracker stored;
	result = myOuterTracker;
	if (result != null) {
		result.innerDirtied(myMaxDirty);
		result.innerTrulyDirtied(myTrulyDirtySoFar);
		result.innerDirtyInfos(myDirtyInfos);
		result.reportProblems();
	}
	if (myFileName != null) {
		if (TheTrackerList == null || ((stored = TheTrackerList.fetchMatch(this)) == null)) {
			myOuterTracker = TheTrackerList;
			myDirtyInfos = MuSet.make();
			TheTrackerList = this;
		}
		else {
			stored.updateFrom(this);
		}
	}
	return result;
/*
udanax-top.st:13268:CBlockTracker methodsFor: 'accessing'!
{CBlockTracker | NULL} fetchUnwrapped
	
	| result {CBlockTracker | NULL} stored {CBlockTracker | NULL} |
	result _ myOuterTracker.
	result ~~ NULL
		ifTrue: 
			[result innerDirtied: myMaxDirty.
			result innerTrulyDirtied: myTrulyDirtySoFar.
			result innerDirtyInfos: myDirtyInfos.
			result reportProblems].
	myFileName ~~ NULL
		ifTrue: 
			[(TheTrackerList == NULL 
			  or: [(stored _ TheTrackerList fetchMatch: self) == NULL])
				ifTrue: 
					[myOuterTracker _ TheTrackerList.
					myDirtyInfos _ MuSet make.
					TheTrackerList _ self]
				ifFalse: [stored updateFrom: self]].
	^result!
*/
}
public void track(String fileName, int lineNo) {
	myFileName = fileName;
	myLineNo = lineNo;
/*
udanax-top.st:13289:CBlockTracker methodsFor: 'accessing'!
{void} track: fileName {char star} with: lineNo {Int32}
	myFileName _ fileName.
	myLineNo _ lineNo.!
*/
}
public void printAllOn(PrintWriter oo) {
	oo.print(this);
	oo.print("\n"+
"");
	if (myOuterTracker != null) {
		myOuterTracker.printAllOn(oo);
	}
/*
udanax-top.st:13296:CBlockTracker methodsFor: 'printing'!
{void} printAllOn: oo {ostream reference} 
	
	oo << self << '
'.
	myOuterTracker ~~ NULL
		ifTrue: [myOuterTracker printAllOn: oo]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("\"");
	oo.print(myFileName);
	oo.print("\"");
	oo.print(", line ");
	oo.print(myLineNo);
	oo.print(": ");
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myMaxDirty);
	oo.print(",	");
	oo.print(myLimit);
	oo.print(",	");
	oo.print(myDirtySoFar);
	oo.print(",	");
	oo.print(myTrulyDirtySoFar);
	oo.print(", ");
	oo.print(myDirtyInfosCount);
	oo.print(", ");
	oo.print(myOccurencesCount);
	oo.print(")");
/*
udanax-top.st:13303:CBlockTracker methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << '"' << myFileName << '"' << ', line ' << myLineNo << ': ' << self getCategory name << '('.
	oo << myMaxDirty << ',	' 
		<< myLimit << ',	' 
		<< myDirtySoFar << ',	' 
		<< myTrulyDirtySoFar << ', ' 
		<< myDirtyInfosCount << ', '
		<< myOccurencesCount << ')'!
*/
}
public int dirtyInfosCount() {
	return myDirtyInfosCount;
/*
udanax-top.st:13315:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} dirtyInfosCount
	^myDirtyInfosCount!
*/
}
public int dirtySoFar() {
	return myDirtySoFar;
/*
udanax-top.st:13319:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} dirtySoFar
	^myDirtySoFar!
*/
}
public CBlockTracker fetchMatch(CBlockTracker other) {
	if (myFileName != null && (other.fileName() != null && ((myFileName.compareTo(other.fileName())) == 0 && (myLineNo == other.lineNo())))) {
		return this;
	}
	else {
		if (myOuterTracker == null) {
			return null;
		}
		else {
			return myOuterTracker.fetchMatch(other);
		}
	}
/*
udanax-top.st:13323:CBlockTracker methodsFor: 'private: accessing'!
{CBlockTracker | NULL} fetchMatch: other {CBlockTracker} 
	
	(myFileName ~~ NULL
	 and: [other fileName ~~ NULL
	 and: [(String strcmp: myFileName with: other fileName) = Int32Zero 
	 and: [myLineNo = other lineNo]]])
		ifTrue: [^self]
		ifFalse: 
			[myOuterTracker == NULL
				ifTrue: [^NULL]
				ifFalse: [^myOuterTracker fetchMatch: other]]!
*/
}
public String fileName() {
	return myFileName;
/*
udanax-top.st:13335:CBlockTracker methodsFor: 'private: accessing'!
{char star | NULL} fileName
	^myFileName!
*/
}
public void innerDirtied(int dirty) {
	myDirtySoFar = myDirtySoFar + dirty;
/*
udanax-top.st:13339:CBlockTracker methodsFor: 'private: accessing'!
{void} innerDirtied: dirty {IntegerVar} 
	
	myDirtySoFar _ myDirtySoFar + dirty!
*/
}
public void innerDirtyInfos(MuSet dirties) {
	myDirtyInfos.storeAll(dirties);
	myDirtyInfosCount = myDirtyInfos.count();
/*
udanax-top.st:13343:CBlockTracker methodsFor: 'private: accessing'!
{void} innerDirtyInfos: dirties {MuSet of: IntegerPos}
	myDirtyInfos storeAll: dirties.
	myDirtyInfosCount _ myDirtyInfos count!
*/
}
public void innerTrulyDirtied(int dirty) {
	myTrulyDirtySoFar = myTrulyDirtySoFar + dirty;
/*
udanax-top.st:13348:CBlockTracker methodsFor: 'private: accessing'!
{void} innerTrulyDirtied: dirty {IntegerVar}
	myTrulyDirtySoFar _ myTrulyDirtySoFar + dirty!
*/
}
public int limit() {
	return myLimit;
/*
udanax-top.st:13352:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} limit
	^myLimit!
*/
}
public int lineNo() {
	return myLineNo;
/*
udanax-top.st:13356:CBlockTracker methodsFor: 'private: accessing'!
{Int32} lineNo
	^myLineNo!
*/
}
public int maxDirty() {
	return myMaxDirty;
/*
udanax-top.st:13360:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} maxDirty
	^myMaxDirty!
*/
}
public int occurencesCount() {
	return myOccurencesCount;
/*
udanax-top.st:13364:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} occurencesCount
	^ myOccurencesCount!
*/
}
public void reportProblems() {
	return ;
/*
udanax-top.st:13367:CBlockTracker methodsFor: 'private: accessing'!
{void} reportProblems
	
	^VOID
	"(myLimit < 1000 
	 and: [myDirtyInfosCount > myMaxDirty 
	 		""((myDirtySoFar max: myTrulyDirtySoFar) max: myDirtyInfosCount) > myLimit""])
		ifTrue: 
			[cerr << '
Limit exceeded [
'.
			self printAllOn: cerr.
			[cerr endEntry.
			""myDirtyInfosCount > myMaxDirty
				ifTrue: [self halt]""] smalltalkOnly]"!
*/
}
public int slack() {
	return myLimit - myDirtySoFar;
/*
udanax-top.st:13382:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} slack
	^myLimit - myDirtySoFar!
*/
}
public int trulyDirtySoFar() {
	return myTrulyDirtySoFar;
/*
udanax-top.st:13386:CBlockTracker methodsFor: 'private: accessing'!
{IntegerVar} trulyDirtySoFar
	^myTrulyDirtySoFar!
*/
}
public void updateFrom(CBlockTracker other) {
	myMaxDirty = Math.max(myMaxDirty, other.maxDirty());
	myLimit = Math.min(myLimit, other.limit());
	myDirtySoFar = Math.max(myDirtySoFar, other.dirtySoFar());
	myTrulyDirtySoFar = Math.max(myTrulyDirtySoFar, other.trulyDirtySoFar());
	myDirtyInfosCount = Math.max(myDirtyInfosCount, other.dirtyInfosCount());
	myOccurencesCount = myOccurencesCount + other.occurencesCount();
/*
udanax-top.st:13390:CBlockTracker methodsFor: 'private: accessing'!
{void} updateFrom: other {CBlockTracker}
	myMaxDirty _ myMaxDirty max: other maxDirty.
	myLimit _ myLimit min: other limit.
	myDirtySoFar _ myDirtySoFar max: other dirtySoFar.
	myTrulyDirtySoFar _ myTrulyDirtySoFar max: other trulyDirtySoFar.
	myDirtyInfosCount _ myDirtyInfosCount max: other dirtyInfosCount.
	myOccurencesCount _ myOccurencesCount + other occurencesCount!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:13401:CBlockTracker methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static CBlockTracker make(int dirty, CBlockTracker outer) {
	return new CBlockTracker(dirty, outer);
/*
udanax-top.st:13413:CBlockTracker class methodsFor: 'creation'!
make: dirty {IntegerVar} with: outer {CBlockTracker | NULL}
	^self create: dirty with: outer!
*/
}
public static void linkTimeNonInherited() {
	TheTrackerList = null;
/*
udanax-top.st:13419:CBlockTracker class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheTrackerList _ NULL!
*/
}
/**
 * CBlockTracker printTrackersOn: cerr. cerr endEntry
 */
public static void printTrackersOn(PrintWriter oo) {
	oo.print("\n"+
"\n"+
"Consistent-Block Behavior\n"+
"\n"+
"");
	if (TheTrackerList != null) {
		TheTrackerList.printAllOn(oo);
	}
	oo.print("\n"+
"");
/*
udanax-top.st:13425:CBlockTracker class methodsFor: 'printing'!
{void} printTrackersOn: oo {ostream reference} 
	"CBlockTracker printTrackersOn: cerr. cerr endEntry"
	
	oo << '
Consistent-Block Behavior
'.
	TheTrackerList ~~ NULL
		ifTrue: [TheTrackerList printAllOn: oo].
	oo << '
'.!
*/
}
public CBlockTracker() {
/*

Generated during transformation
*/
}
public CBlockTracker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
