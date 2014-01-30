/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.stacker;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.stacker.StackExaminer;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * main() routines that are going to invoke garbage collection should
 * call StackExaminer::stackEnd(&stackObj), where stackObj is an Int32
 * local to main''s stack frame.  This should be called before anything
 * else, even invoking the Initializer object.
 */
public class StackExaminer extends Heaper {

	protected static int StackEnd;
	protected static PrimPtrTable TheStackSet;
/*
udanax-top.st:52657:
Heaper subclass: #StackExaminer
	instanceVariableNames: ''
	classVariableNames: '
		StackEnd {Int32 star} 
		TheStackSet {PrimPtrTable} '
	poolDictionaries: ''
	category: 'Xanadu-stacker'!
*/
/*
udanax-top.st:52663:
StackExaminer comment:
'main() routines that are going to invoke garbage collection should
call StackExaminer::stackEnd(&stackObj), where stackObj is an Int32
local to main''s stack frame.  This should be called before anything
else, even invoking the Initializer object.'!
*/
/*
udanax-top.st:52668:
(StackExaminer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:52677:
StackExaminer class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:52680:
(StackExaminer getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(StackExaminer.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:52673:StackExaminer methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
/**
 * Do NOT destroy the result table.  It is reused to avoid allocation during purgeClean.
 */
public static PrimPtrTable pointersOnStack() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:52685:StackExaminer class methodsFor: 'accessing'!
{PrimPtrTable} pointersOnStack
	"Do NOT destroy the result table.  It is reused to avoid allocation during purgeClean."
	| result {PrimPtrTable} |
	TheStackSet == NULL ifTrue: [TheStackSet := PrimPtrTable make: 1024].
	result _ TheStackSet.
	result clearAll.
	[| context |
	context _ thisContext.
	[context ~~ nil] whileTrue: [
		context stack do: [:p |
			(p == nil or: [p isInteger]) ifFalse: [
				result at: p asOop store: result
			]
		].
		result at: context receiver store: result].
	] smalltalkOnly.
	'Int32 stack;
	Int32 * stackPtr = & stack;
	if (StackExaminer::stackEnd() == NULL) {
		BLAST(StackEndUninitialized);
	}
	for (; stackPtr < (Int32 *) StackExaminer::stackEnd(); stackPtr++) {
		if (*stackPtr !!= 0 && (*stackPtr & 3) == 0) {
			result->store((Int32)(void*)*stackPtr, result);
		}
	}' translateOnly.
	^ result!
*/
}
public static int stackEnd() {
	return StackEnd;
/*
udanax-top.st:52713:StackExaminer class methodsFor: 'accessing'!
{Int32 star INLINE} stackEnd
	^ StackEnd!
*/
}
public static void stackEnd(int end) {
	StackEnd = end;
/*
udanax-top.st:52716:StackExaminer class methodsFor: 'accessing'!
{void} stackEnd: end {Int32 star}
	StackEnd _ end!
*/
}
public static void linkTimeNonInherited() {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:52721:StackExaminer class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	StackEnd := NULL.
	TheStackSet := NULL.!
*/
}
public StackExaminer() {
/*

Generated during transformation
*/
}
public StackExaminer(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
