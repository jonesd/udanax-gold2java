/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.ExceptionRecord;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * myPromise is the number of the promise that caused this error.  It will be the excuse for
 * an Excused promise.
 */
public class ExceptionRecord extends Heaper {

	protected int myPromise;
	protected int myError;
/*
udanax-top.st:18865:
Heaper subclass: #ExceptionRecord
	instanceVariableNames: '
		myPromise {IntegerVar}
		myError {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:18871:
ExceptionRecord comment:
'myPromise is the number of the promise that caused this error.  It will be the excuse for an Excused promise.'!
*/
/*
udanax-top.st:18873:
(ExceptionRecord getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:18916:
ExceptionRecord class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:18919:
(ExceptionRecord getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ExceptionRecord.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Return the error most useful to the client for figuring out what happened.  This returns
 * the earliest cause of an error (typically a broken promise.
 */
public ExceptionRecord best(ExceptionRecord rec) {
	if (rec == null) {
		return this;
	}
	if (rec.promise() <= myPromise) {
		return rec;
	}
	return this;
/*
udanax-top.st:18878:ExceptionRecord methodsFor: 'accessing'!
{ExceptionRecord} best: rec {ExceptionRecord | NULL}
	"Return the error most useful to the client for figuring out what happened.  This returns the earliest cause of an error (typically a broken promise."
	
	rec == NULL ifTrue: [^self].
	rec promise <= myPromise ifTrue: [^rec].
	^self!
*/
}
public int error() {
	return myError;
/*
udanax-top.st:18885:ExceptionRecord methodsFor: 'accessing'!
{Int32} error
	^myError!
*/
}
public boolean isExcused() {
	return myError == ExceptionRecord.excused();
/*
udanax-top.st:18888:ExceptionRecord methodsFor: 'accessing'!
{BooleanVar} isExcused
	^myError == ExceptionRecord excused!
*/
}
public int promise() {
	return myPromise;
/*
udanax-top.st:18891:ExceptionRecord methodsFor: 'accessing'!
{IntegerVar} promise
	^myPromise!
*/
}
public ExceptionRecord(int promise, int error) {
	super();
	myPromise = promise;
	myError = error;
/*
udanax-top.st:18896:ExceptionRecord methodsFor: 'creation'!
create: promise {IntegerVar} with: error {Int32}
	super create.
	myPromise _ promise.
	myError _ error!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myPromise);
	if (myError == ExceptionRecord.excused()) {
		oo.print(", excused)");
	}
	else {
		if (myError == ExceptionRecord.typeMismatch()) {
			oo.print(", typeMismatch)");
		}
		else {
			if (myError == ExceptionRecord.badCategory()) {
				oo.print(", badCategory)");
			}
		}
	}
/*
udanax-top.st:18904:ExceptionRecord methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myPromise.
	myError == ExceptionRecord excused ifTrue: [oo << ', excused)']
	ifFalse: [myError == ExceptionRecord typeMismatch ifTrue: [oo << ', typeMismatch)']
	ifFalse: [myError == ExceptionRecord badCategory ifTrue: [oo << ', badCategory)']]]!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:18912:ExceptionRecord methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static int badCategory() {
	return PromiseManager.problemNumber("BAD_CATEGORY"
	/* BLAST(BAD_CATEGORY) */
	);
/*
udanax-top.st:18924:ExceptionRecord class methodsFor: 'constant'!
{Int32} badCategory
	^PromiseManager problemNumber: 'BAD_CATEGORY' "BLAST(BAD_CATEGORY)"!
*/
}
public static int excused() {
	return PromiseManager.problemNumber("BROKEN_PROMISE"
	/* BLAST(BROKEN_PROMISE) */
	);
/*
udanax-top.st:18927:ExceptionRecord class methodsFor: 'constant'!
{Int32} excused
	^PromiseManager problemNumber: 'BROKEN_PROMISE' "BLAST(BROKEN_PROMISE)"!
*/
}
public static int typeMismatch() {
	return PromiseManager.problemNumber("TYPE_MISMATCH"
	/* BLAST(TYPE_MISMATCH) */
	);
/*
udanax-top.st:18930:ExceptionRecord class methodsFor: 'constant'!
{Int32} typeMismatch
	^PromiseManager problemNumber: 'TYPE_MISMATCH' "BLAST(TYPE_MISMATCH)"!
*/
}
public static int wasNull() {
	return PromiseManager.problemNumber("WAS_NULL"
	/* BLAST(WAS_NULL) */
	);
/*
udanax-top.st:18933:ExceptionRecord class methodsFor: 'constant'!
{Int32} wasNull
	^PromiseManager problemNumber: 'WAS_NULL' "BLAST(WAS_NULL)"!
*/
}
public static ExceptionRecord badCategory(int promise) {
	return new ExceptionRecord(promise, badCategory());
/*
udanax-top.st:18938:ExceptionRecord class methodsFor: 'creation'!
{ExceptionRecord} badCategory: promise {IntegerVar}
	^self create: promise with: self badCategory!
*/
}
public static ExceptionRecord excuse(int promise) {
	return new ExceptionRecord(promise, excused());
/*
udanax-top.st:18941:ExceptionRecord class methodsFor: 'creation'!
{ExceptionRecord} excuse: promise {IntegerVar}
	^self create: promise with: self excused!
*/
}
public static ExceptionRecord mismatch(int promise) {
	return new ExceptionRecord(promise, typeMismatch());
/*
udanax-top.st:18944:ExceptionRecord class methodsFor: 'creation'!
{ExceptionRecord} mismatch: promise {IntegerVar}
	^self create: promise with: self typeMismatch!
*/
}
public static ExceptionRecord wasNull(int promise) {
	return new ExceptionRecord(promise, wasNull());
/*
udanax-top.st:18947:ExceptionRecord class methodsFor: 'creation'!
{ExceptionRecord} wasNull: promise {IntegerVar}
	^self create: promise with: self wasNull!
*/
}
public ExceptionRecord() {
/*

Generated during transformation
*/
}
public ExceptionRecord(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
