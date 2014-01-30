/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snfinfo;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snfinfo.SpecialistRcvrJig;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A tool to read partial packets from the disk to measure statistics.
 */
public class SpecialistRcvrJig extends Heaper {

/*
udanax-top.st:52631:
Heaper subclass: #SpecialistRcvrJig
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-snfinfo'!
*/
/*
udanax-top.st:52635:
SpecialistRcvrJig comment:
'A tool to read partial packets from the disk to measure statistics.'!
*/
/*
udanax-top.st:52637:
(SpecialistRcvrJig getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:52646:
SpecialistRcvrJig class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:52649:
(SpecialistRcvrJig getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SpecialistRcvrJig.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:52642:SpecialistRcvrJig methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static Category receiveCategory(Rcvr rcvr) {
	return ((SpecialistRcvr) rcvr).fetchStartOfInstance();
/*
udanax-top.st:52654:SpecialistRcvrJig class methodsFor: 'receiving'!
{Category} receiveCategory: rcvr {Rcvr}
	^ (rcvr cast: SpecialistRcvr) fetchStartOfInstance.!
*/
}
public SpecialistRcvrJig() {
/*

Generated during transformation
*/
}
public SpecialistRcvrJig(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
