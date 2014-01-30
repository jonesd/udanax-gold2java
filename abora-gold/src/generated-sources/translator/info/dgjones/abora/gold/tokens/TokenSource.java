/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tokens;

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tokens.TokenSource;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Manage a set of integerVars as tokens.  The Available array is tokens that have been
 * returned to the pool.  They get used in preference to allocating new ones so that we keep
 * the numbers dense.
 */
public class TokenSource extends Heaper {

	protected Int32Array myAvailable;
	protected int myAvailableCount;
	protected int myCeiling;
/*
udanax-top.st:62807:
Heaper subclass: #TokenSource
	instanceVariableNames: '
		myAvailable {Int32Array}
		myAvailableCount {Int32}
		myCeiling {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tokens'!
*/
/*
udanax-top.st:62814:
TokenSource comment:
'Manage a set of integerVars as tokens.  The Available array is tokens that have been returned to the pool.  They get used in preference to allocating new ones so that we keep the numbers dense.'!
*/
/*
udanax-top.st:62816:
(TokenSource getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
/*
udanax-top.st:62861:
TokenSource class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:62864:
(TokenSource getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TokenSource.class).setAttributes( new Set().add("CONCRETE").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public void returnToken(int token) {
	if (token == myCeiling) {
		myCeiling = myCeiling - 1;
		return ;
	}
	if (myAvailableCount >= myAvailable.count()) {
		myAvailable = (Int32Array) (myAvailable.copyGrow(myAvailableCount + 1));
	}
	myAvailable.storeInt(myAvailableCount, token);
	myAvailableCount = myAvailableCount + 1;
/*
udanax-top.st:62821:TokenSource methodsFor: 'accessing'!
{void} returnToken: token {Int32}
	token == myCeiling ifTrue: [
		myCeiling _ myCeiling - 1.
		^VOID
	].
	myAvailableCount >= myAvailable count ifTrue: [
	 	myAvailable _ (myAvailable copyGrow: myAvailableCount+1) cast: Int32Array.
	].
	myAvailable at: myAvailableCount storeInt: token.
	myAvailableCount _ myAvailableCount + 1.!
*/
}
public int takeToken() {
	int tmp;
	if (myAvailableCount > 0) {
		myAvailableCount = myAvailableCount - 1;
		tmp = myAvailable.intAt(myAvailableCount);
	}
	else {
		myCeiling = myCeiling + 1;
		tmp = myCeiling;
	}
	/* Removed smalltalkOnly */
	return tmp;
/*
udanax-top.st:62832:TokenSource methodsFor: 'accessing'!
{Int32} takeToken
|tmp {Int32}|
	myAvailableCount > Int32Zero ifTrue: [
		myAvailableCount _ myAvailableCount - 1.
		tmp _myAvailable intAt: myAvailableCount.
		
	] ifFalse: [
		myCeiling _ myCeiling + 1.
		tmp _myCeiling
	].
	[tmp == NULL ifTrue:[self halt]] smalltalkOnly.
	
	^tmp!
*/
}
public TokenSource() {
	super();
	myAvailable = Int32Array.make(10);
	myAvailableCount = 0;
	myCeiling = 0;
/*
udanax-top.st:62848:TokenSource methodsFor: 'creation'!
create
	super create.
	myAvailable _ Int32Array make: 10.
	myAvailableCount _ Int32Zero.
	myCeiling _ Int32Zero!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:62856:TokenSource methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:62858:TokenSource methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static TokenSource make() {
	return new TokenSource();
/*
udanax-top.st:62869:TokenSource class methodsFor: 'creation'!
make
	^TokenSource create!
*/
}
public TokenSource(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
