/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.basic;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.Calc;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xpp.basic.CalcCreator;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class CalcCreator extends Heaper {

/*
Xanadu-Xpp-Basic.st:2873:
Heaper subclass: #CalcCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
*/
/*
Xanadu-Xpp-Basic.st:2879:
(CalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #BY.PROXY; add: #DEFERRED; add: #EQ; yourself)!
*/
/*
Xanadu-Xpp-Basic.st:2896:
CalcCreator class
	instanceVariableNames: ''!
*/
/*
Xanadu-Xpp-Basic.st:2899:
(CalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #BY.PROXY; add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CalcCreator.class).setAttributes( new Set().add("BYPROXY").add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * return a new calculator
 */
public Calc newCalc() {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-Basic.st:2884:CalcCreator methodsFor: 'proxy accessing'!
{Calc PROXY} newCalc
	"return a new calculator"
	self subclassResponsibility!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
Xanadu-Xpp-Basic.st:2891:CalcCreator methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
Xanadu-Xpp-Basic.st:2893:CalcCreator methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void cleanupGarbage() {
	Smalltalk.atPut(CALC_CUISINE, null);
/*
Xanadu-Xpp-Basic.st:2904:CalcCreator class methodsFor: 'smalltalk: init'!
cleanupGarbage
	CalcCuisine _ NULL!
*/
}
public static void initTimeNonInherited() {
	Cookbook.declareCookbook("calc", AboraSupport.findCategory(CalcCreator.class), Smalltalk.associationAt(CALC_CUISINE).refValue(), Smalltalk.associationAt(XPP_CUISINE).refValue());
/*
Xanadu-Xpp-Basic.st:2907:CalcCreator class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	Cookbook declareCookbook: 'calc' with: CalcCreator with: CalcCuisine with: XppCuisine!
*/
}
public static void linkTimeNonInherited() {
	Recipe.defineGlobal(CALC_CUISINE, null);
/*
Xanadu-Xpp-Basic.st:2910:CalcCreator class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	Recipe star defineGlobal: #CalcCuisine with: NULL.!
*/
}
public CalcCreator() {
/*

Generated during transformation
*/
}
public CalcCreator(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
