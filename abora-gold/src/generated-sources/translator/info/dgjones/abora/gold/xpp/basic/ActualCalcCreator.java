/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.basic;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.ActualCalc;
import info.dgjones.abora.gold.java.missing.Calc;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.ActualCalcCreator;
import info.dgjones.abora.gold.xpp.basic.CalcCreator;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ActualCalcCreator extends CalcCreator {

/*
Xanadu-Xpp-Basic.st:2913:
CalcCreator subclass: #ActualCalcCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
*/
/*
Xanadu-Xpp-Basic.st:2919:
(ActualCalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
Xanadu-Xpp-Basic.st:2936:
ActualCalcCreator class
	instanceVariableNames: ''!
*/
/*
Xanadu-Xpp-Basic.st:2939:
(ActualCalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ActualCalcCreator.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * return a new calculator
 */
public Calc newCalc() {
	return new ActualCalc(20);
/*
Xanadu-Xpp-Basic.st:2924:ActualCalcCreator methodsFor: 'accessing'!
{Calc PROXY} newCalc
	"return a new calculator"
	^ActualCalc create: 20!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
Xanadu-Xpp-Basic.st:2931:ActualCalcCreator methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
Xanadu-Xpp-Basic.st:2933:ActualCalcCreator methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static CalcCreator make() {
	return new ActualCalcCreator();
/*
Xanadu-Xpp-Basic.st:2944:ActualCalcCreator class methodsFor: 'pseudo-constructor'!
{CalcCreator} make
	^self create!
*/
}
public ActualCalcCreator() {
/*

Generated during transformation
*/
}
public ActualCalcCreator(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
