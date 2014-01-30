/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.settable;

import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.collection.basic.SharedPtrArray;
import info.dgjones.abora.gold.collection.settable.BucketArrayStepper;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class BucketArrayStepper extends TableStepper {

	protected TableEntry myEntry;
	protected SharedPtrArray myEntries;
	protected int myNextBucket;
	protected static InstanceCache SomeSteppers;
/*
udanax-top.st:55515:
TableStepper subclass: #BucketArrayStepper
	instanceVariableNames: '
		myEntry {TableEntry | NULL}
		myEntries {SharedPtrArray of: TableEntry}
		myNextBucket {Int4}'
	classVariableNames: 'SomeSteppers {InstanceCache} '
	poolDictionaries: ''
	category: 'Xanadu-Collection-SetTable'!
*/
/*
udanax-top.st:55522:
(BucketArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:55594:
BucketArrayStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55597:
(BucketArrayStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BucketArrayStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper fetch() {
	if (myEntry == null) {
		return null;
	}
	else {
		return myEntry.value();
	}
/*
udanax-top.st:55527:BucketArrayStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	myEntry == NULL ifTrue: [^NULL] ifFalse: [^myEntry value]!
*/
}
public boolean hasValue() {
	return myEntry != null;
/*
udanax-top.st:55530:BucketArrayStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myEntry ~~ NULL!
*/
}
public void step() {
	if (myEntry != null) {
		myEntry = myEntry.fetchNext();
		verifyEntry();
	}
/*
udanax-top.st:55533:BucketArrayStepper methodsFor: 'operations'!
{void} step
	myEntry ~~ NULL ifTrue: [
		myEntry _ myEntry fetchNext.
		self verifyEntry]!
*/
}
/**
 * Step through the bucket till we find something with a matching key.
 */
public void verifyEntry() {
	int bucket;
	/* use a local index to avoid pointer refs in loop */
	if (myEntry != null) {
		return ;
	}
	bucket = myNextBucket;
	while (bucket < myEntries.count()) {
		Heaper nextEntry;
		nextEntry = myEntries.fetch(bucket);
		bucket = bucket + 1;
		if (nextEntry != null) {
			myEntry = (TableEntry) nextEntry;
			myNextBucket = bucket;
			return ;
		}
	}
	/* Tranformed - include a shareLess here [TransformBucketArrayStepperVerifyEntry] */
	myEntries.shareLess();
	myEntries = null;
/*
udanax-top.st:55540:BucketArrayStepper methodsFor: 'private:'!
{void} verifyEntry
	"Step through the bucket till we find something with a matching key."
	| bucket {Int32} |
	"use a local index to avoid pointer refs in loop"
	myEntry ~~ NULL ifTrue: [^VOID].
	bucket _ myNextBucket.
	[bucket < myEntries count] whileTrue:
		[|nextEntry {Heaper wimpy}|
		nextEntry _ myEntries fetch: bucket.
		bucket _ bucket + 1.
		nextEntry ~~ NULL ifTrue:
			[myEntry _ nextEntry cast: TableEntry.
			myNextBucket _ bucket.
			^VOID]].
	myEntries := NULL.!
*/
}
public int index() {
	if ( ! (myEntry != null)) {
		throw new AboraAssertionException("Illegal access");
	}
	return myEntry.index();
/*
udanax-top.st:55558:BucketArrayStepper methodsFor: 'special'!
{IntegerVar} index
	myEntry ~~ NULL assert: 'Illegal access'.
	^myEntry index!
*/
}
public Position position() {
	if ( ! (myEntry != null)) {
		throw new AboraAssertionException("Illegal access");
	}
	return myEntry.position();
/*
udanax-top.st:55562:BucketArrayStepper methodsFor: 'special'!
{Position} position
	myEntry ~~ NULL assert: 'Illegal access'.
	^myEntry position!
*/
}
public BucketArrayStepper(SharedPtrArray entries, TableEntry entry, int nextBucket) {
	super();
	myEntry = entry;
	myEntries = entries;
	myNextBucket = nextBucket;
	myEntries.shareMore();
	verifyEntry();
/*
udanax-top.st:55568:BucketArrayStepper methodsFor: 'protected: create'!
create: entries {SharedPtrArray} with: entry {TableEntry | NULL} with: nextBucket {Int32}
	super create.
	myEntry _ entry.
	myEntries _ entries.
	myNextBucket _ nextBucket.
	myEntries shareMore.
	self verifyEntry!
*/
}
public void destruct() {
	if (myEntries != null) {
		myEntries.shareLess();
	}
	super.destruct();
/*
udanax-top.st:55576:BucketArrayStepper methodsFor: 'protected: create'!
{void} destruct
	myEntries ~~ NULL ifTrue: [
		myEntries shareLess].
	super destruct!
*/
}
public Stepper copy() {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new BucketArrayStepper(myEntries, myEntry, myNextBucket);
	}
	else {
		return 
		/* TODO newBecome */
		new BucketArrayStepper(myEntries, myEntry, myNextBucket);
	}
/*
udanax-top.st:55583:BucketArrayStepper methodsFor: 'create'!
{Stepper} copy
	| result {Heaper} |
	result := SomeSteppers fetch.
	result ==  NULL
		ifTrue: [^BucketArrayStepper create: myEntries with: myEntry with: myNextBucket]
		ifFalse: [^(BucketArrayStepper new.Become: result) create: myEntries with: myEntry with: myNextBucket]!
*/
}
public void destroy() {
	if ( ! (SomeSteppers.store(this))) {
		super.destroy();
	}
/*
udanax-top.st:55590:BucketArrayStepper methodsFor: 'create'!
{void} destroy
	(SomeSteppers store: self) ifFalse: [super destroy]!
*/
}
public static void initTimeNonInherited() {
	SomeSteppers = InstanceCache.make(8);
/*
udanax-top.st:55602:BucketArrayStepper class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	SomeSteppers := InstanceCache make: 8!
*/
}
public static void linkTimeNonInherited() {
	SomeSteppers = null;
/*
udanax-top.st:55605:BucketArrayStepper class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	SomeSteppers := NULL!
*/
}
public static TableStepper make(SharedPtrArray entries) {
	Heaper result;
	result = SomeSteppers.fetch();
	if (result == null) {
		return new BucketArrayStepper(entries, null, 0);
	}
	else {
		return 
		/* TODO newBecome */
		new BucketArrayStepper(entries, null, 0);
	}
/*
udanax-top.st:55610:BucketArrayStepper class methodsFor: 'creation'!
{TableStepper} make: entries {SharedPtrArray}
	| result {Heaper} |
	result := SomeSteppers fetch.
	result == NULL
		ifTrue: [^self create: entries with: NULL with: Int32Zero]
		ifFalse: [^(self new.Become: result) create: entries with: NULL with: Int32Zero]!
*/
}
public BucketArrayStepper() {
/*

Generated during transformation
*/
}
public BucketArrayStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
